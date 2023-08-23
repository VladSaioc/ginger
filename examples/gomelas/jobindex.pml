// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0

#define default true
#define def_var_callOpts119  ?? // mand callOpts line 119
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
typedef Mutexdef {
	chan Lock = [0] of {bool};
	chan Unlock = [0] of {bool};
	chan RLock = [0] of {bool};
	chan RUnlock = [0] of {bool};
	int Counter = 0;}



init {
	chan child_Remove220 = [1] of {int};
	run Remove22(child_Remove220);
	child_Remove220?0;
stop_process:skip
}

proctype Remove22(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousRemove1211210 = [1] of {int};
	chan productKey_mutex = [1] of {int};
	int var_callOpts = def_var_callOpts119; // opt var_callOpts


	if 	:: true ->
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;


	if 	:: true ->
		goto stop_process
	:: true;
	fi;


	if 	:: true ->


		if 		:: true ->
			goto stop_process
		:: true;
		fi
	:: true;
	fi;


	if 	:: true ->
		chan errChan_ch = [var_callOpts] of {int};
				for(i : 0.. var_callOpts-1) {
			for10: skip;
			run AnonymousRemove121121(errChan_ch,productKey_mutex,child_AnonymousRemove1211210);
			run receiver(child_AnonymousRemove1211210);
			for10_end: skip
		};
		for10_exit: skip;


		if
		:: 0 != -2 && var_callOpts-1 != -3 ->
						for(i : 0.. var_callOpts-1) {
				for21: skip;
								errChan_ch?0;
				for21_end: skip
			};
			for21_exit: skip
		:: else ->
			do
			:: true ->
				for20: skip;
								errChan_ch?0;
				for20_end: skip
			:: true ->
				break
			od;
			for20_exit: skip
		fi
	:: true;
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymousRemove121121(chan errChan_ch;chan productKey_mutex;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	errChan_ch!0;
	stop_process: skip;
	child!0
}

proctype receiver(chan c) {
	c?0
}
