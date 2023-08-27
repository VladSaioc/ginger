// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_events41  ?? // mand events line 41
#define ub_for38_2  -2
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
	chan child_TestEventBox140 = [1] of {int};
	run TestEventBox14(child_TestEventBox140);
	child_TestEventBox140?0;
stop_process:skip
}

proctype TestEventBox14(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousTestEventBox20180 = [1] of {int};
	int var_events = def_var_events41; // opt var_events
	chan ch_ch = [0] of {int};
	run AnonymousTestEventBox2018(ch_ch,child_AnonymousTestEventBox20180);
	run receiver(child_AnonymousTestEventBox20180);


	for i := 0; i < x; i++ {
		ch_ch?0;
	}
	for i := 0; i < x; i++ {
		ch_ch!0;
	}
	stop_process: skip;
	child!0
}
proctype AnonymousTestEventBox2018(chan ch_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;

	for i := 0; i < y; i++ {
		ch_ch!0;
	}
	for i := 0; i < y; i++ {
		ch_ch?0;
	}
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype receiver(chan c) {
	c?0
}
