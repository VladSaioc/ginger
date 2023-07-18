// num_comm_params=2
// num_mand_comm_params=2
// num_opt_comm_params=0

// git_link=https://github.com//blob///home/au/PhD_Materials/RHUL/benchmarks/perkeep/pkg/blobserver/shard/shard.go#L86
#define default true
#define def_var_blobs  ?? // mand blobs line 86
#define def_var_m92  ?? // mand m line 92
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
	chan child_batchedShards860 = [1] of {int};
	run batchedShards86(def_var_blobs,child_batchedShards860);
	child_batchedShards860?0;
stop_process:skip
}

proctype batchedShards86(int var_blobs;chan child) {
	bool closed; 
	bool ok; 
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousbatchedShards96920 = [1] of {int};
	int var_m = def_var_m92; // opt var_m
	chan ch_ch = [var_m] of {int};
		for(i : 0.. var_m-1) {
		for20: skip;
		run AnonymousbatchedShards9692(ch_ch,child_AnonymousbatchedShards96920);
		run receiver(child_AnonymousbatchedShards96920);
		for20_end: skip
	};
	for20_exit: skip;
	

	if /* 	/home/au/PhD_Materials/RHUL/benchmarks/perkeep/pkg/blobserver/shard/shard.go:101:2 */
	:: var_m-1 != -3 -> 
				for(i : 0.. var_m-1) {
			for30: skip;
						ch_ch?0;
			for30_end: skip
		};
		for30_exit: skip
	:: else -> 
		do
		:: true -> 
			for31: skip;
						ch_ch?0;
			for31_end: skip
		:: true -> 
			break
		od;
		for31_exit: skip
	fi;
	goto stop_process;
	stop_process: skip;
	child!0
}
proctype AnonymousbatchedShards9692(chan ch_ch;chan child) {
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
