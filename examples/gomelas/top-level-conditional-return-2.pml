// num_comm_params=1
// num_mand_comm_params=1
// num_opt_comm_params=0


#define default true
#define def_var_gids991  ?? // mand gids line 991
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
	chan child_ExportOverNetwork9680 = [1] of {int};
	run ExportOverNetwork968(child_ExportOverNetwork9680);
	child_ExportOverNetwork9680?0;
stop_process:skip
}

proctype ExportOverNetwork968(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousExportOverNetwork9939930 = [1] of {int};
	int var_gids = def_var_gids991; // opt var_gids
	chan ch_ch = [var_gids] of {int};


	if
	:: true ->
		goto stop_process
	:: true;
	fi;


	if
	:: true ->
		goto stop_process
	:: true;
	fi;
		for(i : 0.. var_gids-1) {
		for10: skip;
		run AnonymousExportOverNetwork993993(ch_ch,child_AnonymousExportOverNetwork9939930);
		run receiver(child_AnonymousExportOverNetwork9939930);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && var_gids-1 != -3 ->
				for(i : 0.. var_gids-1) {
			for21: skip;
						ch_ch?0;


			if
			:: true ->
				goto stop_process
			:: true;
			fi;
			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						ch_ch?0;


			if
			:: true ->
				goto stop_process
			:: true;
			fi;
			for20_end: skip
		:: true ->
			break
		od;
		for20_exit: skip
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymousExportOverNetwork993993(chan ch_ch;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
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
