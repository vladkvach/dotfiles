#!/bin/sh

progress_bar () {
	_progress=$((("${1}"*100/100*100)/100))
	_done=$((("${_progress}"*4)/10))
	_left=$((40-$_done))
	_done=$(printf "%${_done}s")
	_left=$(printf "%${_left}s")
    _spin='-\|/'
    i=0    
    while kill -0 $! >/dev/null 2>&1; do
        i=$(( (i+1) %4 ))
        printf "\rProgress : [$(printf '%s' "$_done" | sed -r 's/ /#/g')$(printf '%s' "$_left" | sed -r 's/ /-/g')] ${_progress}%%\t$(printf '%s' "$_spin" | cut -c $((i+1)))"
        sleep .1
    done
}

sysctl () {
	echo "" >> /etc/sysctl.conf
	cat <<- EOF >> /etc/sysctl.conf
	# Personal settings
	kern.vt.enable_bell=0
	hw.syscons.bell=0
	kern.ipc.shmmax=67108864
	kern.ipc.shmall=32768
	kern.sched.preempt_thresh=224
	vfs.usermount=1
	EOF
}

loader () {
	echo "" >> /boot/loader.conf
	cat <<- EOF >> /boot/loader.conf
	# Personal settings
	loader_logo="beastie"
	kern.ipc.shmseg=1024
	kern.ipc.shmmni=1024
	kern.maxproc=100000
	mmc_load="YES"
	mmcsd_load="YES"
	sdhci_load="YES"
	fuse_load="YES"
	coretemp_load="YES"
	tmpfs_load="YES"
	aio_load="YES"
	libiconv_load="YES"
	libmchain_load="YES"
	cd9660_iconv_load="YES"
	msdosfs_iconv_load="YES"
	acpi_video_load="YES"
	acpi_ibm_load="YES"
	fusefs_load="YES"
	EOF
}

rc () {
	echo "" >> /etc/rc.conf
	cat <<- EOF >> /etc/rc.conf
	# Personal settings
	powerd_enable="YES"
	powerd_flags="-a hiadaptive -b adaptive"
	hcsecd_enable="YES"
	sdpd_enable="YES"
	ntpd_enable="YES"
	ntpd_flags="-g"
	linux_enable="YES"
	pf_enable="YES"
	dbus_enable="YES"
	kld_list="/boot/modules/i915kms.ko"
	EOF
}

fstab () {
	echo "" >> /etc/fstab
	cat <<- EOF >> /etc/fstab
	# Personal settings
	devfs              /compat/linux/dev            devfs           rw,late                                0  0
	linprocfs          /compat/linux/proc           linprocfs       rw,late                                0  0
	linsysfs           /compat/linux/sys            linsysfs        rw,late                                0  0
	fdescfs            /compat/linux/dev/fd         fdescfs         rw,late,linrdlnk                       0  0
	tmpfs              /compat/linux/dev/shm        tmpfs           rw,late,size=1g,mode=1777              0  0
	EOF

	mount /compat/linux/dev
	mount /compat/linux/dev/shm
	mount /compat/linux/dev/fd
	mount /compat/linux/proc
	mount /compat/linux/sys

	service linux start
	service pf start
}

doas () {
	cat <<- EOF >> /usr/local/etc/doas.conf
	permit nopass keepenv :wheel
	permit nopass keepenv root as root
	EOF
}

create_user () {
    read -rep $'\nEnter username: ' username

	if id -u $username >/dev/null 2>&1; then
		read -p "User $username already exists! Remove user? (Yes/No)"$'\nAnswer: ' answer
		lo_answer=$(echo $answer | tr '[:upper:]' '[:lower:]')
		if $lo_answer = "yes"; then
			rmuser -y $username >/dev/null 2>&1
		fi
		create_user
    else
		read -p "Enter password : " password
        up_username=$(echo $username | tr '[:lower:]' '[:upper:]')
        
		if pw user add -n $username -c $up_username -d /home/$username -g wheel -G video -s /bin/sh -m >/dev/null 2>&1; then
			if echo $password | pw usermod -n $username -h 0 >/dev/null 2>&1; then
				echo "User has been added to system!"
				xorg_cfg $username
			else
				echo "Failed set user pass!"
                rmuser -y $username >/dev/null 2>&1
				create_user
			fi
		else
            echo "Failed add a user!"
            create_user
        fi
    fi
}

xorg_cfg () {
    cat /usr/local/etc/X11/xinit/xinitrc | tail -r | sed -e '1,6d' | tail -r > /home/${1}/.xinitrc
    echo "slock  xssG-lock -- slock &" >> /home/${1}/.xinitrc
    echo "setxkbmap -layout us,ru -option grp:ctrl_shift_toggle -option ctrl:nocaps" >> /home/${1}/.xinitrc
    echo "xrandr --output eDP-1 --primary --mode 1920x1080 --rate 60.00 --pos 0x0 --rotate normal --output DP-1 --mode 1920x1080 --rate 60.00 --pos 0x0 --rotate left --right-of eDP-1" >> /home/${1}/.xinitrc
    echo "exec dwm" >> /home/${1}/.xinitrc
}

pre_install () {
    freebsd-update fetch && freebsd-update install
    portsnap fetch && portsnap extract
    pkg update -y && pkg upgrade
}

main () {
    pkg_list="fuse fusefs-ntfs fusefs-exfat fusefs-ext2 git emacs emulators/linux_base-c7 drm-kmod libva-intel-driver xorg xkb-switch doas textproc/hs-pandoc editors/libreoffice alsa-utils intel-backlight www/npm pdf-tools texlive-full zip unzip ncurses nerd-fonts"
    installation_steps="sysctl loader rc fstab doas create_user"
	increment=$((100/($(echo "$pkg_list" | wc -w) + 8)))
	unknown_packages=""
    perc_comp=0
    
	progress_bar $perc_comp
	pre_install >/dev/null 2>&1 &
	perc_comp=$((perc_comp+increment))
    
    for word in $pkg_list; do
        if pkg search "$word" >/dev/null 2>&1; then
            pkg install -y "$word" >/dev/null 2>&1 &
            perc_comp=$((perc_comp+increment))
        else
            unknown_packages="${unknown_packages} $word"
        fi
        progress_bar "$perc_comp"
    done

	if [ "$(echo "$unknown_packages" | wc -w)" -eq 0 ]; then
		for step in $installation_steps; do
			if [ "$step" = "create_user" ]; then
                printf '\007'
                sleep 0.5 >/dev/null 2>&1 &
				progress_bar 100
				$step
			else
				$step >/dev/null 2>&1 &
				progress_bar $((perc_comp=$((perc_comp+increment))))
			fi
		done
		printf "\n\nConfiguration completed!\n\n"
		exit 0
	else
		unknown_packages=$(echo "$unknown_packages" | sed 's/[^[:space:],]\+/"&"/g')
        printf "\n\nConfiguration error!\n\nMissing packages: %s\n\n" "$unknown_packages"
		exit 2
	fi
}

if [ $(id -u) -eq 0 ]; then
    printf "System Setup!\n"
	main
else
	printf "Only the 'root' user can configure the system!!!"
	exit 1
fi
