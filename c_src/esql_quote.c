

/* niftest.c */
#include "erl_nif.h"

int quote_str(int size, unsigned char* ori, unsigned char* dist);


static ERL_NIF_TERM quote(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    if (enif_inspect_iolist_as_binary(env, argv[0], &bin))
    {
        unsigned int count = 0;
        size_t newsize;
        ErlNifBinary newbin;

        for (unsigned int i = 0; i < bin.size; i++)
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

int quote_str(int size, unsigned char* ori, unsigned char* dist)
{
    int j = 0;
    dist[j++] = '\'';
    for(int i = 0; i < size; i++)
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

static ErlNifFunc nif_funcs[] =
{
    {"quote", 1, quote}
};

ERL_NIF_INIT(esql_quote,nif_funcs,NULL,NULL,NULL,NULL)