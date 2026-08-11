(ns cljs.test)

(type-record test-env
  (report-counters :map<keyword;int>)
  (testing-vars :list<string>)
  (testing-contexts :list<string>)
  (reporter :keyword))

(type-record test-location
  (file :string)
  (line :int)
  (column :option<int>))

(type-variant test-action
  (SynchronousTest :fn<bool>)
  (AsyncTest :fn<fn<bool>;bool>)
  (DeferredTest :fn<test-action>)
  (TestBlock :list<test-action>))

(type-record registered-test
  (registered-test-name :string)
  (registered-test-action :test-action))

(signature cljs.test/*current-env*
  :option<test-env>)
(signature cljs.test/empty-env
  :overload<fn<test-env>;fn<keyword;test-env>>)
(signature cljs.test/get-current-env
  :fn<test-env>)
(signature cljs.test/set-env!
  :fn<test-env;test-env>)
(signature cljs.test/clear-env!
  :fn<option<test-env>>)
(signature cljs.test/get-and-clear-env!
  :fn<test-env>)
(signature cljs.test/inc-report-counter!
  :fn<keyword;test-env>)
(signature cljs.test/testing-contexts-str
  :fn<string>)
(signature cljs.test/testing-vars-str
  :fn<test-location;string>)
(signature cljs.test/test-env-value
  :fn<map<keyword;int>;list<string>;list<string>;keyword;test-env>)
(signature cljs.test/replace-current-env!
  :fn<test-env;map<keyword;int>;list<string>;test-env>)
(signature cljs.test/push-testing-context!
  :fn<string;test-env>)
(signature cljs.test/pop-testing-context!
  :fn<test-env>)
(signature cljs.test/registered-tests
  :ref<map<string;list<registered-test>>>)
(signature cljs.test/register-test!
  :fn<string;string;fn<bool>;registered-test>)
(signature cljs.test/register-test-action!
  :fn<string;string;test-action;registered-test>)
(signature cljs.test/run-registered-test!
  :fn<registered-test;test-env>)
(signature cljs.test/run-registered-tests!
  :fn<list<string>;test-env>)
(signature cljs.test/run-single-test!
  :fn<string;fn<bool>;test-env>)
(signature cljs.test/run-block [storage]
  :fn<seqable<test-action;storage>;bool>)
(signature cljs.test/run-block-then
  :fn<seq<test-action>;fn<bool>;bool>)
(signature cljs.test/async?
  :fn<test-action;bool>)
(signature cljs.test/synchronous-test-action
  :fn<fn<bool>;test-action>)
(signature cljs.test/async-test-action
  :fn<fn<fn<bool>;bool>;test-action>)
(signature cljs.test/deferred-test-action
  :fn<fn<test-action>;test-action>)
(signature cljs.test/async-testing-action
  :fn<string;fn<test-action>;test-action>)
(signature cljs.test/block [storage]
  :fn<seqable<test-action;storage>;test-action>)
(signature cljs.test/registered-test-step
  :fn<string;fn<bool>;test-action>)
(signature cljs.test/namespace-test-action
  :fn<string;test-action>)
(signature cljs.test/test-var-block
  :fn<fn<bool>;list<test-action>>)
(signature cljs.test/test-var
  :fn<fn<bool>;bool>)
(signature cljs.test/test-vars-block [storage]
  :fn<seqable<fn<bool>;storage>;seq<test-action>>)
(signature cljs.test/test-vars [storage]
  :fn<seqable<fn<bool>;storage>;bool>)
(signature cljs.test/is-result
  :fn<bool;bool>)

(signature cljs.test/compose-fixtures [result]
  :fn<fn<fn<result>;result>;fn<fn<result>;result>;fn<fn<result>;result>>)
(signature cljs.test/join-fixtures [result storage]
  :fn<seqable<fn<fn<result>;result>;storage>;fn<fn<result>;result>>)
(signature cljs.test/successful?
  :fn<map<keyword;int>;bool>)
