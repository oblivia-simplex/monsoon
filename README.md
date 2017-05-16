# Monsoon
## A Binary Rainfall Packet Capture Visualizer
### Based on a description found in Greg Conti's _Security Data Visualization_

#### Images from a Live Capture:
Heatmap mode:
![heatmap](https://github.com/oblivia-simplex/monsoon/raw/master/images/heatmap.png)

Bitwise mode:
![bitwise](https://github.com/oblivia-simplex/monsoon/raw/master/images/bitwise.png)

Bytewise mode:
![bytewise](https://github.com/oblivia-simplex/monsoon/raw/master/images/bytewise.png)

Run make to compile. You may need sudo privileges in order to
give monsoon packet capture permissions. 

### Dependencies:
* SDL
* libpcap
* quicklisp
* Steel Bank Common Lisp (sbcl)

There's a precompiled binary in build/monsoon. Be sure to give it
capture permissions with
```
make perms
```
or
```
sudo setcap cap_net_raw,cap_net_admin=eip build/monsoon
```

### Interactive commands:
* pressing a numeric key n will tell the pcap listener to capture
  (2 ^ (n-1)) packets at a time, before processing. This can help
  if monsoon seems to be lagging (use a higher n) or stuttering
  (use a lower n).
* pressing '-' will set the capture rate to maximum
* press ESCAPE to quit
* press 'a' to toggle ASCII highlighting
* press 'b' to toggle between bitwise (blue) and bytewise (green)
  representation


Better documentation is on the way!

* Conti's tool is also described in [this slideshow](https://www.blackhat.com/presentations/bh-europe-06/bh-eu-06-Conti/bh-eu-06-conti.pdf), given at Black Hat. 
