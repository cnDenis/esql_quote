

/* niftest.c */
#include "erl_nif.h"
#include "string.h"

int quote_str(size_t size, unsigned char* ori, unsigned char* dist);

static ERL_NIF_TERM quote(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (enif_inspect_iolist_as_binary(env, argv[0], &bin))
    {
        size_t count = 0;
        size_t i = 0;
        size_t newsize;
        ErlNifBinary newbin;

        for (i = 0; i < bin.size; i++)
        {
            switch (bin.data[i])
            {
                case '\0':
                case '\b':
                case '\t':
                case '\n':
                case '\r':
                case 26: /* \Z */
                case '\"':
                case '\'':
                case '\\':
                    count++;
            }
        }

        newsize = bin.size + count + 2;
        enif_alloc_binary(newsize, &newbin);
        quote_str(bin.size, bin.data, newbin.data);
        return enif_make_binary(env, &newbin);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

int quote_str(size_t size, unsigned char* ori, unsigned char* dist)
{
    size_t i = 0;
    size_t j = 0;
    dist[j++] = '\'';
    for(i = 0; i < size; i++)
    {
        switch (ori[i])
        {
            case '\0':
                dist[j++] = '\\';
                dist[j++] = '0';
                break;
            case '\b':
                dist[j++] = '\\';
                dist[j++] = 'b';
                break;
            case '\t':
                dist[j++] = '\\';
                dist[j++] = 't';
                break;
            case '\n':
                dist[j++] = '\\';
                dist[j++] = 'n';
                break;
            case '\r':
                dist[j++] = '\\';
                dist[j++] = 'r';
                break;
            case 26: /* \Z */
                dist[j++] = '\\';
                dist[j++] = 'Z';
                break;
            case '\"':
                dist[j++] = '\\';
                dist[j++] = '"';
                break;
            case '\'':
                dist[j++] = '\\';
                dist[j++] = '\'';
                break;
            case '\\':
                dist[j++] = '\\';
                dist[j++] = '\\';
                break;
            default:
                dist[j++] = ori[i];
                break;
        }
    }
    dist[j] = '\'';
    return 0;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"quote", 1, quote}
};

ERL_NIF_INIT(esql_quote,nif_funcs,on_load,NULL,on_upgrade,NULL)
