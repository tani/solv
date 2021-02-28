all: solv

data: cpc.pl ipc.pl solv.pl
	tar -zcf data $^

solv: data header.bash
	cat header.bash data > $@
	chmod +x $@

.PHONY: clean

clean:
	rm data solv
