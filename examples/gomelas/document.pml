// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_docMapNYC181  ?? // mand docMapNYC line 181
typedef Chandef {
	chan sync = [0] of {bool};
	chan enq = [0] of {bool};
	chan deq = [0] of {bool,bool};
	chan sending = [0] of {bool};
	chan rcving = [0] of {bool};
	chan closing = [0] of {bool};
	int size = 0;
	int num_msgs = 0;
	bool closed = false;
}



init {
	chan child_createDocumentsNYCWithBib1750 = [1] of {int};
	run createDocumentsNYCWithBib175(child_createDocumentsNYCWithBib1750);
	child_createDocumentsNYCWithBib1750?0;
stop_process:skip
}

proctype createDocumentsNYCWithBib175(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymouscreateDocumentsNYCWithBib1851850 = [1] of {int};
	int var_docMapNYC = def_var_docMapNYC181; // opt var_docMapNYC
	chan createDocChan_ch = [var_docMapNYC] of {int};
		for(i : 0.. var_docMapNYC-1) {
		for10: skip;
		run AnonymouscreateDocumentsNYCWithBib185185(createDocChan_ch,child_AnonymouscreateDocumentsNYCWithBib1851850);
		run receiver(child_AnonymouscreateDocumentsNYCWithBib1851850);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && var_docMapNYC-1 != -3 ->
				for(i : 0.. var_docMapNYC-1) {
			for21: skip;
						createDocChan_ch?0;
			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						createDocChan_ch?0;
			for20_end: skip
		:: true ->
			break
		od;
		for20_exit: skip
	fi;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymouscreateDocumentsNYCWithBib185185(chan ch_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;


	if 	:: true;
	fi;
		ch_ch!0;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
