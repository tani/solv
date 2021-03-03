all: solv

%.swi: %.pl
	cat $^ | perl -nle 'print if not /%-swi/ ... /%-swi/' > $@

%.pro: %.pl
	cat $^ | perl -nle 'print if not /%-gnu/ ... /%-gnu/' > $@

data: src/cpc.swi src/ipc.swi src/solv.swi src/printer.swi
	chmod +x src/solv.swi
	tar -zcf data $^

solv: src/cli.sh data
	cat $^ > $@
	chmod +x $@

.PHONY: clean

clean:
	rm data solv src/*.swi src/*.pro
