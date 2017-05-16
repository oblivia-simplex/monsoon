
BUILDNAME=monsoon-$(shell uname -r | tr ' ' -)

.PHONY: clean capture

all: monsoon capture


monsoon: monsoon.lisp cli.lisp tweakables.lisp packages.lisp monsoon.asd graphics-utils.lisp
	[ -d ~/quicklisp/local-projects/monsoon ] || ln -s ${CURDIR} ~/quicklisp/local-projects
	mkdir -p build
	sbcl --script compile.lisp
	mv build/monsoon build/${BUILDNAME}
	ln -s build/${BUILDNAME} build/monsoon

capture: monsoon
	@echo "[*] Now giving monsoon capture permissions..."
	sudo setcap cap_net_raw,cap_net_admin=eip ${CURDIR}/build/${BUILDNAME}
	@echo "[*] Executable binary is in build/monsoon. Happy sniffing!"
      

clean:
	rm build/*


