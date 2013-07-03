# TODO

- [ ] Multiplication filter/tag
- [ ] Merge to Genapp Core

# Issues

## Database scheme-to-class mappings

How do we ultimately manage mappings like:

```
java_driver(<<"mysql">>) ->
    {<<"com.mysql.jdbc.Driver">>,
     <<"com.mysql.jdbc.jdbc2.optional.MysqlDataSource">>};
java_driver(<<"sql">>) ->
    {<<"com.microsoft.sqlserver.jdbc.SQLServerDriver">>,
     <<"com.microsoft.sqlserver.jdbc.SQLServerDataSource">>};
java_driver(<<"postgres">>) ->
    {<<"org.postgresql.Driver">>,
     <<"org.postgresql.ds.PGSimpleDataSource">>};
java_driver(<<"oracle">>) ->
    {<<"oracle.jdbc.OracleDriver">>,
     <<"oracle.jdbc.pool.OracleDataSource">>}.
```

### What we know

1. CloudBees specific logic must be in an extension

### To decide

1. How is this CloudBees logic implemented?

### Options

1. __Use filters__

2. Use json smart transforms

### Next Step

Get some idea of how hard it will be to implement filters. 
__Relatively easy__

## CLI

```
Maybe something like this:

NAME
	genapp-template-render - render a template

SYNOPSIS
	genapp-template-render [OPTIONS] TEMPLATE FILE

OPTIONS
	-h, --help
	    Print help and exit.

	-c CONTEXT, --context=CONTEXT
	   Use a context whn rendering the template. CONTEXT is in the format
	   NAME:PATH, where NAME is the name of the context and PATH is a path
	   to the context source. Currently only JSON formats are supported.
	   Multiple contexts can be specified by using multiple options.

CONTEXTS
	A context is a hierarchical data structure that can be accessed
	from a template using the Django templating langauge. Each context
	is referenced using its name as specified by the context option.

	Here's a sample template snippet that iterates through each value
	in a "metadata"  associatvie array context:

	   {% for key, value in metadata.items %}
	   key = {{key}}
	   ---------------------------------
	   {{value|pprint}}
	   {% endfor %}

EXAMPLES
	genapp-template-render -c metadata:metadata.json context.xml.in context.xml

```
