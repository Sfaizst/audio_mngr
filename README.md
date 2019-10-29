ReadMe audio_mngr

This Tool is more to be a proof of conecpt, it's not well written, do not listen to code guidelines etc.
I used it just for me and my problems with pulse...

The writing and code style is lazy and probably buggy, please do not take this codestyle as an inspiration.

Screenshoot:
![Alt text](Screenshoot.png?raw=true "Screenshoot")

Features:
- Use cables to connect players, speakers, microphones, etc.
- Use the scrollwheel of your mouse to change the volume of a device
- Add and manage loops and virtual sinks
- Save and load configs
- Change the default speaker and mic via menu

Installation:
- Download the i386 or x64 version of this tool for your PC
- Make sure pulse is installed on your PC
- Mark the the binary as executeable

How to compile:
- Download and install CodeTyphon from https://www.pilotlogic.com/sitejoom/
- Clone the git to your PC
- Open the project-file with Typhon IDE
- Compile it...

Known problems:
- If a Program / Name is used mutible times the program can't tell the difference when loading from config
- The program was written with the german (UTF8) charset in mind and isn't in any way directly connected to pulse
  all actions / readings from pulse are executed via (bash) commands in the background it may be possible that it's not 
  working very well with other charsets

Requirements:
- Installed Pulse Audio server and at most the command pactl, test with "pactl -h"
- The excuteables are build with gtk2 probably some libs are needed if you dont have gtk2 installed.
  If the program is not starting try running it via terminal it should show the missing librarys.


If someone want to rewrite the program in a more prefessional manner please do so.
For me it's working this way... As said it's more like a proof of concept for what pulse can actually do. 

Note: I can't speak english very well, sorry, I just wanted to solve my problem...

Best greetings
Sfaizst / Daniel
