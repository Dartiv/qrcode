#include "erl_nif.h"

#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include "qrcodegen.h"

static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM result;

    if (!enif_make_existing_atom(env, atom, &result, ERL_NIF_LATIN1)) {
        return enif_make_atom(env, atom);
    }

    return result;
}

static ERL_NIF_TERM make_error(ErlNifEnv* env, const char* message)
{
    return enif_make_tuple2(
        env,
        make_atom(env, "error"),
        enif_make_string(env, message, ERL_NIF_LATIN1)
    );
}

static ERL_NIF_TERM encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary binary;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &binary)) {
        return enif_make_badarg(env);
    }

    int correction;
    if (!enif_get_int(env, argv[1], &correction)) {
        return enif_make_badarg(env);
    }

    char text[binary.size + 1];
    memset(text, '\0', sizeof(text));
    strncpy(text, (const char*)binary.data, binary.size);
    // printf("\"%s\"\n", text);

    uint8_t qrcode[qrcodegen_BUFFER_LEN_MAX];
    uint8_t buffer[qrcodegen_BUFFER_LEN_MAX];
    bool success = qrcodegen_encodeText(
        text,
        buffer,
        qrcode,
        correction,
        qrcodegen_VERSION_MIN,
        qrcodegen_VERSION_MAX,
        qrcodegen_Mask_AUTO,
        true
    );

    if (!success) {
        return make_error(env, "Encode process failed");
    }

    int s = qrcodegen_getSize(qrcode);
    ERL_NIF_TERM result[s * s];
    for (int y = 0; y < s; y++) {
        for (int x = 0; x < s; x++) {
            bool module = qrcodegen_getModule(qrcode, x, y);
            result[y * s + x] = module ? enif_make_int(env, 1) : enif_make_int(env, 0);
        }
    }

    return enif_make_tuple2(
        env,
        make_atom(env, "ok"),
        enif_make_list_from_array(env, result, s * s)
    );
}

static ErlNifFunc funcs[] = {
    {"c", 2, encode}
};

ERL_NIF_INIT(qrcode, funcs, NULL, NULL, NULL, NULL)
