// num_comm_params=0
// num_mand_comm_params=0
// num_opt_comm_params=0

// git_link=https://github.com//blob///home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml_internal_test.go#L1575
#define default true
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
	chan child_TestReadWriteCache15750 = [1] of {int};
	run TestReadWriteCache1575(child_TestReadWriteCache15750);
	child_TestReadWriteCache15750?0;
stop_process:skip
}

proctype TestReadWriteCache1575(chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_AnonymousTestReadWriteCache158315810 = [1] of {int};
	Mutexdef lsh_cacheLock;
	Mutexdef lsh_yamlReporter_status_l;
	run mutex_monitor(lsh_yamlReporter_status_l);
	run mutex_monitor(lsh_cacheLock);
	chan ch_ch = [0] of {int};
		for(i : 0.. 20-1) {
		for10: skip;
		run AnonymousTestReadWriteCache15831581(ch_ch,lsh_yamlReporter_status_l,lsh_cacheLock,child_AnonymousTestReadWriteCache158315810);
		run receiver(child_AnonymousTestReadWriteCache158315810);
		for10_end: skip
	};
	for10_exit: skip;


	if
	:: 0 != -2 && 20-1 != -3 ->
				for(i : 0.. 20-1) {
			for21: skip;
						ch_ch?0;
			for21_end: skip
		};
		for21_exit: skip
	:: else ->
		do
		:: true ->
			for20: skip;
						ch_ch?0;
			for20_end: skip
		:: true ->
			break
		od;
		for20_exit: skip
	fi;
	stop_process: skip;
	child!0
}
proctype AnonymousTestReadWriteCache15831581(chan ch_ch;Mutexdef lsh_yamlReporter_status_l;Mutexdef lsh_cacheLock;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	chan child_writeCacheKey4861 = [1] of {int};
	chan child_getCacheKey4790 = [1] of {int};
	run getCacheKey479(lsh_cacheLock,lsh_yamlReporter_status_l,child_getCacheKey4790);
	child_getCacheKey4790?0;
	run writeCacheKey486(lsh_cacheLock,lsh_yamlReporter_status_l,child_writeCacheKey4861);
	child_writeCacheKey4861?0;
	ch_ch!0;
	stop_process: skip;
	child!0
}
proctype getCacheKey479(Mutexdef lsh_cacheLock;Mutexdef lsh_yamlReporter_status_l;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	lsh_cacheLock.RLock?ok /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:480:2 */;
	assert(ok) /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:480:2 */;
	lsh_cacheLock.RUnlock?ok /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:482:2 */;
	assert(ok) /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:482:2 */;
	stop_process: skip;
	child!0
}
proctype writeCacheKey486(Mutexdef lsh_cacheLock;Mutexdef lsh_yamlReporter_status_l;chan child) {
	bool closed;
	bool ok;
	int i;
	bool state = true;
	int num_msgs;
	lsh_cacheLock.Lock?ok /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:487:2 */;
	assert(ok) /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:487:2 */;
	lsh_cacheLock.Unlock?ok /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:490:2 */;
	assert(ok) /* 	/home/user/go-code/bazel-out/k8-fastbuild/bin/src/code.uber.internal/engsec/secret-api-gateway/core/code-uber-internal-engsec-secret-api-gateway-core/src/code.uber.internal/engsec/secret-api-gateway/core/langley_yaml.go:490:2 */;
	stop_process: skip;
	child!0
}

 /* ================================================================================== */
 /* ================================================================================== */
 /* ================================================================================== */

proctype mutex_monitor(Mutexdef m) {
	bool locked = false;
	end: skip;
	do
	:: true ->
		if
		:: m.Counter > 0 ->
			if
			:: m.RUnlock!true;
				m.Counter = m.Counter - 1;
			:: m.RLock!true;
				m.Counter = m.Counter + 1;
			fi;
		:: locked ->
			m.Unlock!true;
			locked = false;
		:: else ->
			end1: skip;
			if
			:: m.Unlock!false;
			:: m.Lock!true -> locked =true;
			:: m.RUnlock!false;
			:: m.RLock!true -> m.Counter = m.Counter + 1;
			fi;
		fi;
	od
}

proctype receiver(chan c) {
	c?0
}
