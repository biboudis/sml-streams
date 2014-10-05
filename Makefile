MLTON 		:= mlton
SRC 		= $(shell pwd)
BUILD 		= $(SRC)/build
BENCHMARKS_DIR 	= $(SRC)/benchmarks
BENCH-SET 	= streams
BENCH 		:= $(BENCH-SET:%=bench-%) 

FLAGS := -verbose 0

.PHONY: dirs
dirs:
	mkdir -p "$(BUILD)"

.streams.ph : streams.*
	$(MAKE) dirs
	@echo 'Type checking streams.'
	"$(MLTON)" $(FLAGS) -stop tc $(SRC)/streams.mlb
	@touch .streams.ph
streams : .streams.ph

$(BENCH): bench-% : %
	$(MAKE) dirs
	@echo 'Building bench-streams.'
	"$(MLTON)" $(FLAGS) -output $(BUILD)/bench-$* $(BENCHMARKS_DIR)/bench-$*.mlb
bench : $(BENCH)

.PHONY: $(BENCH-SET)
