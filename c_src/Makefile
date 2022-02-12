
PROJECT_NIF_NAME=elocaltime_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	LDFLAGS += -framework CoreFoundation

	LDFLAGS +=  -Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_atom \
				-Wl,-U,_enif_is_number \
                -Wl,-U,_enif_get_int \
				-Wl,-U,_enif_get_resource \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_get_ulong \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_tuple \
                -Wl,-U,_enif_make_int \
                -Wl,-U,_enif_make_resource \
                -Wl,-U,_enif_make_ulong \
				-Wl,-U,_enif_alloc \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_alloc_resource \
				-Wl,-U,_enif_release_resource \
                -Wl,-U,_enif_inspect_binary \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_open_resource_type \
				-Wl,-U,_enif_priv_data
endif

CXXFLAGS += -I../_build/deps/cctz/include -DNDEBUG -g -Wextra -Werror -Wno-missing-field-initializers -fno-exceptions -fno-rtti -std=c++11
LDFLAGS += -L../_build/deps/cctz -lcctz -lstdc++
