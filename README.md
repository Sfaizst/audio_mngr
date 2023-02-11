PulseAduio Cable Manager

This tool is more to be a proof of concept, 
it's not well written, 
do not listen to code guidelines etc and is not in any way
connected to the PulseAudio or Pipewire projects it just uses it's CLI tools in the background.

Consider using Helvum if you want to see a better detailed graph,
additionally there might be other tools that are more powerful I currently just don't know about.

I'm just using it to simplify the hassle I have with pavucontrol and the management of multiple sound devices, virtual devices, loops and sources.

It's well tested on Manjaro with the most recent version of Pipewire and Pulse.
The prebuilt binaries in /bin are compiled on Ubuntu 22.04 and should be working with a reasonable / recent libc and gtk2 libs present.

The writing and code style is lazy, without meaningful comments, please do not take anything like this as an inspiration, since I'm actively using this tool I might find bugs sooner or later, but rewriting or refactoring it in any other language is currently a no-no for me.

Basic Usage [Screenshoot]:
![Alt text](Screenshoot.png?raw=true "Screenshoot")

Dark Mode [Screenshoot]:
![Alt text](Screenshoot_DarkMode.png?raw=true "Screenshoot")

Features:
- Use "cables" (lines) to connect sources, speakers, microphones, loops and monitors.
- Add and manage loops and virtual sinks
- Save and load configs
- Change the default speaker and mic via right click

Settings (via "File" --> "Options"):
- Dark Mode (more eye-friendly)
- Option to enable / disable a timer to automatically check for changes
- Option for always stay on top above all other programs
- Option to disable / enable the usage of pipewire
- Option to change the latency of loops (0..65536) in milliseconds (only works before the are created)

Installation:
- Download the i386 or x64 version of this tool from the /bin subdirectory of the git for your PC
- Make sure pulse or pipewire is installed on your PC
- Mark the the binary as executable

Basic usage:
- Start the program, you should see your audio devices as elements on the field
- Every interaction with the program is with the mouse only:
- Left mouseklick:
    - Connect a device with another one, incompatible devices will be marked in red
    - The connection is made via "Drag'n'Drop"
- Middle mousclick:
    - Mutes / Unmutes a device (is shown in gray)
 - Middle mouse-wheel:
 	- Changes the volume of a device
- Right mouseclick:
    - Íf the device is virtual (loop or virtual sink) a popup menu will show up, so that you can delete the device.
    - If a device isn't default you can set it as default
- Other options (Save... / Load... / Default (Speaker / Mic) are in the menu at the top of the program)

Command-Line-Options:
	- ./audio_mngr_x86_64-linux [Filename] [-close]
	- [Filename]: Define a saved file to load your config from (Like Menu -> File -> Load...)
	- [-close]: Automatically closes the program after loading a file

How to compile:
- Download and install CodeTyphon from https://www.pilotlogic.com/sitejoom/
- Clone the git to your PC
- Open the project-file with Typhon IDE
- Compile it...

Known problems:
- The Loading and Saving of configs is probably a little bugged, even more when devices,.. are not present
- On some devices I'm getting problems myself to run my x86_64 build, because of the usage of a too new libc
  on my compilers side, switching to the i386 build works fine for me on probably all machines I'm using this tool on.
- Equalizers may 

Requirements:
- An installed PulseAudio or Pipewire server
	- If PulseAudio is used you probably need to check if the following commands are present:
		- pacmd
		- pactl
	- If you are using Pipewire just pactl is enough.
- The excusable are built with an gtk2 environment probably some libs are needed if you dont have gtk2 installed.
  If the program is not starting try running it via Terminal it should show the missing librarys or more likely a libc error.(as explained in "known problems").

If someone want to rewrite the program in a more professional manner please do so, a little mention would be nice.
For me it's working this way... As said it's more like a proof of concept.

Note: I can't speak english very well, this ReadMe might be a little rough to read.

Wish you the best and some fun with this little tool.
Best greetings
Sfaizst / ZiwwlPulseAduio Cable Manager

This tool is more to be a proof of concept, 
it's not well written, 
do not listen to code guidelines etc and is not in any way
connected to the PulseAudio or Pipewire projects it just uses it's CLI tools in the background.

Consider using Helvum if you want to see a better detailed graph,
additionally there might be other tools that are more powerful I currently just don't know about.

I'm just using it to simplify the hassle I have with pavucontrol and the management of multiple sound devices, virtual devices, loops and sources.

It's well tested on Manjaro with the most recent version of Pipewire and Pulse.
The prebuilt binaries in /bin are compiled on Ubuntu 22.04 and should be working with a reasonable / recent libc and gtk2 libs present.

The writing and code style is lazy, without meaningful comments, please do not take anything like this as an inspiration, since I'm actively using this tool I might find bugs sooner or later, but rewriting or refactoring it in any other language is currently a no-no for me.

Basic Usage [Screenshoot]:
![Alt text](Screenshoot.png?raw=true "Screenshoot")

Dark Mode [Screenshoot]:
![Alt text](Screenshoot_DarkMode.png?raw=true "Screenshoot")

Features:
- Use "cables" (lines) to connect sources, speakers, microphones, loops and monitors.
- Add and manage loops and virtual sinks
- Save and load configs
- Change the default speaker and mic via right click

Settings (via "File" --> "Options"):
- Dark Mode (more eye-friendly)
- Option to enable / disable a timer to automatically check for changes
- Option for always stay on top above all other programs
- Option to disable / enable the usage of pipewire
- Option to change the latency of loops (0..65536) in milliseconds (only works before the are created)

Installation:
- Download the i386 or x64 version of this tool from the /bin subdirectory of the git for your PC
- Make sure pulse or pipewire is installed on your PC
- Mark the the binary as executable

Basic usage:
- Start the program, you should see your audio devices as elements on the field
- Every interaction with the program is with the mouse only:
- Left mouseklick:
    - Connect a device with another one, incompatible devices will be marked in red
    - The connection is made via "Drag'n'Drop"
- Middle mousclick:
    - Mutes / Unmutes a device (is shown in gray)
 - Middle mouse-wheel:
 	- Changes the volume of a device
- Right mouseclick:
    - Íf the device is virtual (loop or virtual sink) a popup menu will show up, so that you can delete the device.
    - If a device isn't default you can set it as default
- Other options (Save... / Load... / Default (Speaker / Mic) are in the menu at the top of the program)

Command-Line-Options:
	- ./audio_mngr_x86_64-linux [Filename] [-close]
	- [Filename]: Define a saved file to load your config from (Like Menu -> File -> Load...)
	- [-close]: Automatically closes the program after loading a file

How to compile:
- Download and install CodeTyphon from https://www.pilotlogic.com/sitejoom/
- Clone the git to your PC
- Open the project-file with Typhon IDE
- Compile it...

Known problems:
- The Loading and Saving of configs is probably a little bugged, even more when devices,.. are not present
- On some devices I'm getting problems myself to run my x86_64 build, because of the usage of a too new libc
  on my compilers side, switching to the i386 build works fine for me on probably all machines I'm using this tool on.
- Equalizers like PulseEffects might trigger weird effects, I'm not into this tools a lot.

Requirements:
- An installed PulseAudio or Pipewire server
	- If PulseAudio is used you probably need to check if the following commands are present:
		- pacmd
		- pactl
	- If you are using Pipewire just pactl is enough.
- The excusable are built with an gtk2 environment probably some libs are needed if you dont have gtk2 installed.
  If the program is not starting try running it via Terminal it should show the missing librarys or more likely a libc error.(as explained in "known problems").

If someone want to rewrite the program in a more professional manner please do so, a little mention would be nice.
For me it's working this way... As said it's more like a proof of concept.

Note: I can't speak english very well, this ReadMe might be a little rough to read.

Wish you the best and some fun with this little tool.
Best greetings
Sfaizst / Ziwwl