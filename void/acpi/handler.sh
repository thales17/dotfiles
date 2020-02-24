#!/bin/sh
# Default acpi script that takes an entry for all actions

# NOTE: This is a 2.6-centric script.  If you use 2.4.x, you'll have to
#       modify it to not use /sys

minspeed=`cat /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_min_freq`
maxspeed=`cat /sys/devices/system/cpu/cpu0/cpufreq/cpuinfo_max_freq`
setspeed="/sys/devices/system/cpu/cpu0/cpufreq/scaling_setspeed"

set $*

PID=$(pgrep dbus-launch)
export USER=$(ps -o user --no-headers $PID)
USERHOME=$(getent passwd $USER | cut -d: -f6)
export XAUTHORITY="$USERHOME/.Xauthority"
for x in /tmp/.X11-unix/*; do
	displaynum=`echo $x | sed s#/tmp/.X11-unix/X##`
	if [ x"$XAUTHORITY" != x"" ]; then
		export DISPLAY=":$displaynum"
	fi
done

case "$1" in
	button/power)
		#echo "PowerButton pressed!">/dev/tty5
		case "$2" in
			PBTN|PWRF)
				logger "PowerButton pressed: $2, shutting down..."
				shutdown -P now
				;;
			*)
				logger "ACPI action undefined: $2" ;;
		esac
		;;
	button/sleep)
		case "$2" in
			SBTN|SLPB)
			# suspend-to-ram
			logger "Sleep Button pressed: $2, suspending..."
			zzz
			;;
			*)
				logger "ACPI action undefined: $2" ;;
		esac
		;;
	ac_adapter)
		case "$2" in
			AC|ACAD|ADP0)
				case "$4" in
					00000000)
						echo -n $minspeed >$setspeed
						#/etc/laptop-mode/laptop-mode start
						;;
					00000001)
						echo -n $maxspeed >$setspeed
						#/etc/laptop-mode/laptop-mode stop
						;;
				esac
				;;
				*)
					logger "ACPI action undefined: $2" ;;
		esac
		;;
	battery)
		case "$2" in
			BAT0)
				case "$4" in
					00000000)
						#echo "offline" >/dev/tty5
						;;
					00000001)
						#echo "online"  >/dev/tty5
						;;
				esac
				;;
			CPU0)
				;;
			*)
				logger "ACPI action undefined: $2" ;;
		esac
		;;
	button/lid)
		case "$3" in
			close)
				# suspend-to-ram
				logger "LID closed, suspending..."
				sudo -iu adam slock &
				sleep 5
				zzz
				;;
			open)
				logger "LID opened"
				;;
			*)
				logger "ACPI action undefined (LID): $2"
				;;
		esac
		;;
	button/volumeup)
		logger "Volume Up"
		sudo -iu adam pamixer -i 5
		;;
	button/volumedown)
		logger "Volume Down"
		sudo -iu adam pamixer -d 5
		;;
	button/mute)
		logger "Volume mute"
		sudo -iu adam pamixer -t
		;;
	video/brightnessdown)
		logger "Brightness down"
		xbacklight -dec 10
		;;
	video/brightnessup)
		logger "Brightness up"
		xbacklight -inc 10
		;;
	*)
		logger "ACPI group/action undefined: $1 / $2"
		;;
esac
