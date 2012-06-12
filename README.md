# Cersei

[![Build Status](https://secure.travis-ci.org/wjlroe/cersei.png?branch=master)](http://travis-ci.org/wjlroe/cersei)

I listen to Jenkins and I tell people what I hear. Or something.

## Design

- JSON string received from websocket
  (`jenkins_websocket_client:ws_onmessage/1`)
- `jenkins_build_info` parses the JSON and fetches full details from jenkins	
- `build_output_parser` parses the console output to calculate status
  ({pass/fail, errors: 4, failures: 2, tests: 42, duration: 35.6s})
  - "42 steps (1 failed, 41 passed)" - add 1 to failures, add 42 to
    total num of tests
  - "81 examples, 3 failures" - add 3 to failures, add 81 to total num
    of tests
- `build_filter` filters project -> groups
  - groups can be pre-defined (foobar ->
    [foobar-backend, foobar-frontend, foobar-ios])
  - groups can be ad-hoc defined in the client
  - client can subscribe to any group (but only one)
  - project is implicitly a group
- `group_stats` contains state on every group's stats at present
  - last test run results
  - on change -> update all subscribers
- `build_group_websocket_server` sends messages to all subscribed
  clients on a change in a group's stats
  
