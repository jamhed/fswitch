Fswitch
=======

A complimentary Erlang library for FreeSWITCH [mod_erlang_event](https://freeswitch.org/confluence/display/FREESWITCH/mod_erlang_event)
See supplied [etc/erlang.xml](etc/erlang.xml) as a dialplan example.

Usage
=====

```sh
rebar3 get-deps
rebar3 compile
```

Usage and design
================

With `mod_erlang_event` loaded FreeSWITCH becomes an Erlang node, with an ability to handle calls in another Erlang node.
In order to do that one requires to forward calls to FreeSWITCH erlang application specifying full name of the target node.
```xml
<action application="erlang" data="call_sup:! reach@172.17.0.1"/>
```
This produces a message in form in target erlang node sent to a registered process named `call_sup`: `{get_pid, UUID, Ref, From}`,
where `UUID` is a call UUID.

`call_sup` process on this message spawns a call handling process (see `src/call.erl`), and requests initial dump of variables
generating syntetic call event `SYNC` on success.

In spite of all messages sent from `mod_erlang_event` being Erlang strings, this library converts everything to Erlang binaries
(probably we change `mod_erlang_event` too to communcate binary strings only).

Notes
=====

This is currently work in progress, and a foundation of SIP functional testing application [busytone](https://github.com/jamhed/busytone).
