import re
from typing import List, Union, Tuple, Any, Callable, TypeVar, Mapping
from typing.re import Pattern

import tempfile, os.path

Predicate = Union[str,
        Tuple[Pattern, ...],
        Callable[[Any], bool]]

def _string_contains(line, string):
    return string in line

def _pattern_value_match(line, tup):
    pat = tup[0]
    vals = tup[1:]
    m = pat.search(line)
    return m is not None and all(
            v is None or g == v
            for g,v in zip(m.groups(), vals))

def _apply_func(line, func):
    return func(line)

def find_line(lines: List[str], substr: Predicate, start: int = 0) -> int:
    """Find the line that contains or matches substr since line ``start``. """
    if isinstance(substr, str):
        pred = _string_contains
    elif isinstance(substr, tuple):
        pred = _pattern_value_match
    else:
        pred = _apply_func

    for i in range(start, len(lines)):
        if pred(lines[i], substr):
            return i

    raise KeyError("Not found: " + str(substr) + "\n text:" + str(lines) )

def extract_lines(parent: str, begin: Predicate, end: Predicate) -> str:
    """
    Extract the lines between the line containing ``begin`` and the line
    containing ``end`` (excluding both lines) in ``parent``.
    """
    lines = parent.splitlines()

    begin_line = find_line(lines, begin)
    end_line = find_line(lines, end, begin_line+1)

    new_lines = lines[begin_line+1:end_line]

    return "\n".join(new_lines)

def inject_lines(parent: str, begin: Predicate, end: Predicate, generated: str) -> str:
    """
    Replace the lines between the line containing ``begin`` and the line
    containing ``end`` (excluding both lines) in ``parent`` with ``generated``.
    """
    lines = parent.splitlines()

    begin_line = find_line(lines, begin)
    end_line = find_line(lines, end, begin_line+1)

    new_lines = lines[:begin_line+1] + generated.splitlines() + lines[end_line:]

    return "\n".join(new_lines)

STANDARD_PREFIX_BEGIN = "GEN:BEGIN:"
STANDARD_PREFIX_END   = "GEN:END:"

class StandardInjectableFile(object):
    def __init__(self, path: str, injection_points: List[str] = None):
        self.path = path
        if injection_points is None:
            injection_points = []
        self.injection_points = injection_points

    def inject_many(self, m: Mapping[str, str], force=False):
        with open(self.path) as f:
            orig_txt = f.read()

        txt = orig_txt

        for inj_point, inj_content in m.items():
            if inj_point not in self.injection_points and not force:
                raise Exception("Unknown injection point '{}'".format(inj_point))
            inj_begin = STANDARD_PREFIX_BEGIN + inj_point
            inj_end   = STANDARD_PREFIX_END   + inj_point

            new_txt = inject_lines(txt, inj_begin, inj_end, inj_content)
            txt = new_txt

        if not txt.endswith("\n"):
            txt += "\n"

        with tempfile.NamedTemporaryFile("w", delete=False) as f:
            print("Backup to temporary file: {} -> {}".format(self.path, f.name))
            f.write(orig_txt)

        with open(self.path, "w") as f:
            print("Writing to file: {}".format(self.path))
            f.write(txt)

def make_injectable_file_set(
        root_path: str,
        items    : List[Tuple[str, str, List[str]]],
        ) -> Mapping[str, StandardInjectableFile]:
    m = InjectableFileSet()
    for name, path, inj_points in items:
        full_path = os.path.join(root_path, path)
        sif = StandardInjectableFile(full_path, inj_points)
        m[name] = sif
    return m
        
class InjectableFileSet(dict):
    def inject_many(self, m: Mapping[str, Mapping[str, str]]):
        for name, mm in m.items():
            self[name].inject_many(mm)

