import gdb.printing

CLOSURE_TAG = 247
STRING_TAG = 252
DOUBLE_TAG = 253

class ValuePrinter:
    """Pretty prints a value_type."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        if self.val["i"] % 2 == 1:
            # integer
            return str(int(self.val["i"]) >> 1)
        else:
            # block
            return BlockPrinter(self.val["block"].dereference()).to_string()


class BlockPrinter:
    """Pretty prints a block."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        size = self.val["size"]
        tag = self.val["tag"]

        if tag == CLOSURE_TAG:
            return "<closure>"
        elif tag == STRING_TAG:
            return str(self.val["data"][0]["str"])
        elif tag == DOUBLE_TAG:
            return str(self.val["data"][0]["fl"])
        else:
            values = []
            for i in range(int(size)):
                val = self.val["data"][i]["value"]
                values.append(ValuePrinter(val).to_string())
            return "[{}: {}]".format(tag, " ".join(values))


class AnyPrinter:
    """Pretty prints an any_type."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "<any>"


class ClosurePrinter:
    """Pretty prints a closure_type."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        return "<closure>"


class CustomRegexpPrettyPrinter(gdb.printing.RegexpCollectionPrettyPrinter):
    def __init__(self, name):
        super().__init__(name)

    def __call__(self, val):
        typename = str(val.type)

        for printer in self.subprinters:
            if printer.enabled and printer.compiled_re.search(typename):
                return printer.gen_printer(val)

        return None

def build_pretty_printer():
    pp = CustomRegexpPrettyPrinter("observable_ocaml")
    pp.add_printer("value_type", r"^(union )?_?value_type$", ValuePrinter)
    pp.add_printer("block_type", r"^(struct )? _?block_type$", BlockPrinter)
    pp.add_printer("any_type", r"^(union )?_?any_type$", AnyPrinter)
    pp.add_printer("closure_type", r"^(struct )?_?closure_type(\s*\*?)$", ClosurePrinter)
    return pp


gdb.printing.register_pretty_printer(gdb.current_objfile(), build_pretty_printer())
