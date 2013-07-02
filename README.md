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

1. Use filters

2. Use json smart transforms

### Next Step

Get some idea of how hard it will be to implement filters.
