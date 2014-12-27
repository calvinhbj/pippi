Pippi
=====

Pippi is a framework include dynamic data model on nitrogen for erlang.

Need R17+ for maps.

Features:

1. Build on nitrogen framework
2. Dyanmic form model(ets/dets/riak)
3. Good message for utf8 user: Don't need to map model from english to utf8
4. A built in logic model with prolog
5. A good toolkits for nitrogen

Todo:

1. Workflow base on prolog
2. Built in account and right model
3. Comunication model like xmpp
4. Rest service and websocket service for mobile
5. riak backend

## With nitrogen framework
nitrogen is a great framework for erlang, you can find it from <http://nitrogenproject.com>

## Dyanmic form model
### Simple example
step1: add pippi to your rebar.config
``` erlang
    {pippi,         ".*",   {git, "git://github.com/homeway/pippi",            {branch, master}}},
```
step2: create model
``` erlang
    pp:model_set({contact, all, [
        {"姓名", []},
        {"账户", []},
        {"密码", [{type, password}]},
        {"邮箱", []},
        {"朋友", [{type, tags}]}
    ]}).
```
As you see, you can create a from through `model_set`.<br/>
In fact there is a group method to create a modle, just like `modle_clone`,`model_patch`, `model_clone`, `model_filter` and `model_cut`.<br/>

step3: create a form
``` erlang
  pp:form(contact, all).
```
Or you can put some default value to the form:
``` erlang
  pp:form(contact, show, #{"姓名" => "石群杰", "朋友" => ["王二", "张三"]}),
```

### Backend
Pippi need a k/v storage like ets/dets/mnesia/riak/mongodb/couchdb, and so on.<br/>
The `pp_db_adapter_ets` and `pp_db_adapter_dets` is built in now, these is a good choice for test you code.<br/>
