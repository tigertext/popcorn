Welcome to *Popcorn*, an aggregator and analyzer for your server logs.
Its purpose is to provide a single interface where you can see what's
going on in your code, what's happening with your servers and quickly
detect problems caused by changes.

## <a name='TOC'>Table of Contents</a>

  1. [Overview](#overview)
  1. [Features](#features)
  1. [Configuration](#configuration)
  1. [Requirements](#requirements)
  1. [Usage](#usage)

## <a name='overview'>Overview</a>

  Popcorn is a log aggregator and analyzer for your server logs. Its
  purpose is to provide a single interface where you can see what's 
  going on in your code, what's happening with your servers and quickly 
  detect problems caused by changes.

## <a name='features'>Features</a>

  - Send logs in realtime using UDP, don't block your running app  
  - Data encoded using protocol buffers for efficiency  
  - Log messages are categorized by node, node type, node version, 
    severity and date
  - Easily connect your app log messages from many platforms with 
    available clients

## <a name='configuration'>Configuration</a>

  Add popcorn as a dependency to an existing Erlang/OTP app by 
  adding a reference in the rebar.config file.

  Add popcorn as a dependency to an existing Erlang/OTP app by 
  adding a reference in the rebar.config file.

  Your app should supply a configuration for popcorn, which includes 
  the user authentication policy.  Possible options in the config are:

  ```erlang
    {popcorn, [
          {udp_listen_port, 9125},
          {http_listen_port, 9125},
          {http_auth_enabled, true},
          {http_auth_db_type, config},
          {http_auth_users, [{<<"log_user">>, <<"log_password">>}]},
          {log_retention, [{<<"debug">>,      {hours, 2}},
                           {<<"info">>,       {hours, 2}},
                           {<<"notice">>,     {hours, 2}},
                           {<<"warn">>,       {hours, 2}},
                           {<<"error">>,      {months, 1}},
                           {<<"critical">>,   {months, 1}},
                           {<<"alert">>,      {months, 1}},
                           {<<"emergency">>,  {months, 1}}]}
          ]} 
  ```

## <a name='requirements'>Requirements</a>

  - Erlang R15B01 or later is required (for Cowboy dependency)

## <a name='usage'>Usage</a>

  By default, the server will listen on port 9125 (both UDP and HTTP), 
  so after running, visit http://{host}:9125 and you should see a login 
  screen.  



Thanks to the following:

https://github.com/littke/inspiritas-bootstrap/
https://github.com/eternicode/bootstrap-datepicker

