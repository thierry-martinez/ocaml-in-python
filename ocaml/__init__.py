"""ocaml-in-python"""

import collections.abc
import ctypes
import os

int = int
float = float
string = str
bool = bool
bytes = bytes

def error_this_function_should_be_implemented_in_ocaml():
    raise NotImplementedError("This function should be implemented in OCaml")

class __list_api:
    make = None
    make_from_sequence = None
    length = None
    getitem = None

class abstract:
    """ocaml.abstract"""

    def __init__(self, *params, **kw):
        try:
            self._capsule = kw["__capsule"]
        except KeyError:
            raise RuntimeError("abstract type cannot be constructed")

def print_value(v):
    """double quote v if v is a string, return str(v) otherwise"""
    if isinstance(v, str):
        quoted = v.replace('"', r'\"')
        return f'"{quoted}"'
    else:
        return str(v)

class list(collections.abc.Sequence):
    """ocaml.list"""

    def _api_for_type(self, _type):
        error_this_function_should_be_implemented_in_ocaml()
        return self

    _default_type = None

    _default_length = None

    _field_names = None

    def __init__(self, *params, **kw):
        try:
            capsule = kw["__capsule"]
        except KeyError:
            capsule = None
        if capsule is None:
            try:
                length = kw["len"]
                if self._default_length not in (None, length):
                    raise IndexError(
                        f"len={length} but {self._default_length} expected"
                    )
            except KeyError:
                length = self._default_length
            nb_params = len(params)
            if nb_params == 1:
                self._template_item = params[0]
                self._items = None
                self._length = length
            elif nb_params == 0 and self._field_names != None:
                self._items = [kw[field] for field in self._field_names]
            else:
                if length not in (None, nb_params):
                    raise IndexError(
                        f"len={length} but {nb_params} items given"
                    )
                self._template_item = None
                self._items = params
                self._length = nb_params
            try:
                type_ = kw["type"]
            except KeyError:
                type_ = self._default_type
            if type_ is None:
                self._capsule = None
                self._api = None
            else:
                self._api = self._api_for_type(type_)
                self._init_from_api()
        else:
            self._capsule = capsule
            try:
                self._api = kw["api"]
            except KeyError:
                self._api = self._api_for_type(self._default_type)

    def _init_from_api(self):
        if self._items is None:
            self._capsule = self._api.make(
                self._length, self._template_item
            )
            del self._template_item
        else:
            self._capsule = self._api.make_from_sequence(self._items)
            del self._items

    def __len__(self):
        if self._capsule is None:
            return self._length
        return self._api.length(self._capsule)

    def __getitem__(self, index):
        if self._capsule is None:
            if self._items is None:
                if 0 <= index < self._length:
                    return self._template_item
                raise IndexError(
                    f"index {index} out of bounds (0<=.<{self._length})"
                )
            return self._items[index]
        return self._api.getitem(self._capsule, index)

    def _get_type(self):
        if self._capsule is None:
            raise RuntimeError("Type is unknown yet")
        return self._api.get_type()

    def _set_api(self, api):
        self._api = api
        self._init_from_api()
        return self._capsule

    def __getattr__(self, name):
        if name[0:1] == "f":
            try:
                index = int(name[1:])
            except ValueError:
                index = None
            if index is not None:
                return self[index]
        raise AttributeError(f"Unknown field {name}")

    def __repr__(self):
        return "[" + ",".join([repr(item) for item in self]) + "]"

    def __str__(self):
        return "[" + ";".join([print_value(item) for item in self]) + "]"

class tuple(list):
    """ocaml.tuple"""

    def __repr__(self):
        return "(" + ",".join([repr(item) for item in self]) + ")"

    def __str__(self):
        return "(" + ",".join([print_value(item) for item in self]) + ")"

class __array_api(__list_api):
    setitem = None

class array(list):
    """ocaml.array"""

    def __setitem__(self, index, value):
        if self._capsule is None:
            if self._items is None:
                if self._length is None:
                    self._items = [self._template_item]
                else:
                    self._items = [self._template_item] * self._length
                del self._template_item
            self._items[index] = value
        else:
            self._api.setitem(self._capsule, index, value)

    def __repr__(self):
        return "[" + ",".join([repr(item) for item in self]) + "]"

    def __str__(self):
        return "[|" + ";".join([print_value(item) for item in self]) + "|]"

class record(array):
    """ocaml.record"""

    def __repr__(self):
        return "{" + ",".join(
            [repr(field) + ":" + repr(value)
                for (field, value) in zip(self._field_names, self)]) + "}"

    def __str__(self):
        return "{" + ";".join(
            [str(field) + "=" + print_value(value)
                for (field, value) in zip(self._field_names, self)]) + "}"

class variant(array):
    """ocaml.variant"""

    _constructor_name = None

    def __repr__(self):
        if len(self) == 0:
            return self._constructor_name
        elif self._field_names is None:
            return self._constructor_name + "(" + ",".join(
                [print_value(item) for item in self]) + ")"
        else:
            return (self._constructor_name + "(" + ",".join([
                str(field) + "=" + repr(value)
                for (field, value) in zip(self._field_names, self)]) + ")")

    def __str__(self):
        if len(self) == 0:
            return self._constructor_name
        elif self._field_names is None:
            return self._constructor_name + "(" + ",".join(
                [print_value(item) for item in self]) + ")"
        else:
            return (self._constructor_name + " {" + ";".join([
                str(field) + "=" + print_value(value)
                for (field, value) in zip(self._field_names, self)]) + "}")


class bytes(array):
    """ocaml.bytes"""

    _default_type = []

    def __repr__(self):
        return repr(self._api.to_string(self._capsule))

    def __str__(self):
        return self._api.to_string(self._capsule)

class option(variant):
    """ocaml.option"""

class Some(option):
    """ocaml.Some"""

    _constructor_name = "Some"

    _default_length = 1

class exn(variant, Exception):
    """ocaml.exn"""

def __initialize_ocaml():
    curdir = os.path.dirname(os.path.realpath(__file__))
    dll = ctypes.PyDLL(f"{curdir}/ocaml_in_python.so", ctypes.RTLD_GLOBAL)
    argv_t = ctypes.c_char_p * 2
    argv = argv_t("python".encode('utf-8'), None)
    dll.caml_startup(argv)

__initialize_ocaml()
