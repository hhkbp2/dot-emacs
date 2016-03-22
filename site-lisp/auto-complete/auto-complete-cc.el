;;; auto-complete-cc.el --- Settings for `auto-complete' in c/c++ modes
;; -*- Emacs-Lisp -*-

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2016-03-22 11:09>

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(require 'auto-complete)


;;; define source for c standard library keywords

(defun ac-libc-candidates ()
  "A keyword candidates produce function for c standard library."
  '(;;
    ;; * 1 integer functions
    ;; 1.1 arithmetic operation
    ;; #include <stdlib.h>
    ;; int abs(int value);
    ;; long int labs(long int value);
    ;; div_t div(int numerator, int denominator);
    ;; ldiv_t ldiv(long int numerator, long int denom);
    "abs" "labs"
    "div" "ldiv"
    ;; 1.2 random number
    ;; #include <stdlib.h>
    ;; int rand(void);
    ;; void srand(unsigned int seed);
    "rand" "srand"
    ;; 1.3 string conversion
    ;; #include <stdlib.h>
    ;; int atoi(char const *string);
    ;; long int atol(char const *string);
    ;; long int strtol(char const *string, char **unused, int base);
    ;; unsigned long int strtoul(char const *string, char **unused, int base);
    "atoi" "atol" "strtol" "strtoul"
    ;;
    ;; * 2 floating-point functions
    ;; #include <math.h>
    ;; double sin(double angle);
    ;; double cos(double angle);
    ;; double tan(double angle);
    ;; double asin(double value);
    ;; double acos(double value);
    ;; double atan(double value);
    ;; double atan2(double x, double y);
    "sin" "cos" "tan" "asin" "acos" "atan" "atan2"
    ;; double sinh(double angle);
    ;; double cosh(double angle);
    ;; double tanh(double angle);
    "sinh" "cosh" "tanh"
    ;; double exp(double x);
    ;; double log(double x);
    ;; double log10(double x);
    "exp" "log" "log10"
    ;; double frexp(double value, int *exponent);
    ;; double ldexp(double fraction, int exponent);
    ;; double modf(double value, double *ipart);
    "frexp" "ldexp" "modf"
    ;; double pow(double x, double y);
    ;; dboule sqrt(double x);
    "pow" "sqrt"
    ;; double floor(double x);
    ;; double ceil(double x);
    ;; double fabs(double x);
    ;; double fmod(double x, double y);
    "floor" "ceil" "fabs" "fmod"
    ;; #include <stdlib.h>
    ;; double atof(char const *string);
    ;; double strtod(char const *string, char **unused);
    "atof" "strtod"
    ;;
    ;; * 3 date and time
    ;; 3.1 cpu time
    ;; #include <time.h>
    "clock_t" "CLOCKS_PER_SEC"
    ;; clock_t clock(void);
    "clock"
    ;; 3.2 date time
    "time_t"
    ;; time_t time(time_t *returned_value);
    "time"
    ;;
    ;; TODO
    ;; #include <stdarg.h>
    ;; macro
    "va_start" "va_arg" "va_end"
    ;; type
    "va_list"
    ;;
    ;; functions
    ;; #include <ctype.h>
    "isalnum" "isalpha" "iscntrl" "isdigit" "isgraph" "islower"
    "isprint" "ispunct" "isspace" "isupper" "isxdigit"
    "tolower" "toupper"
    ;; #include <stdlib.h>
    "abort"
    ;; #include <string.h>
    "strlen" "strcmp" "strcat" "strcpy"
    "strncat" "strncpy" "strncmp"
    ;; #define _GNU_SOURCE
    "strnlen"
    ;; #include <assert.h>
    "assert"
    "__FILE__" "__LINE__" "__TIME__" "__DATE__"
    ;;
    ;; macros and variables
    ;; #include <stdlib.h>
    "NULL" "EXIT_SUCCESS" "EXIT_FAILURE" "NDEBUG"
    ;;
    ;; types, enums and structs
    ;; #include <stddef.h>
    "size_t" "ptrdiff_t"
    ;;
    ;; define gcc addtional feature
    "_GNU_SOURCE"
    ))


(ac-define-source libc
  '((candidates . ac-libc-candidates)
    ;; all tip character of customized additional ac source are capitalized
    (symbol . "C")
    (cache)))


;;; define source for c++ standard library keywords

(defun ac-libcpp-candidates ()
  "A keyword candidates produce function for c++ standard library."
  '(;;
    ;; * standard namespace
    "std"
    ;;
    ;; * standard library container
    "string" "bitset"
    ;; sequencial container
    "vector" "list" "deque"
    ;; sequencianlcontainer adaptor
    "stack" "queue" "priority_queue"
    ;; associative container
    "map" "set" "multimap" "multiset"
    ;; preliminary pair type
    "pair" "make_pair" "first" "second"
    ;; sequencial container typedefs
    "size_type" "difference_type" "value_type" "reference" "const_reference"
    ;; sequencial container method
    "begin" "end" "rbegin" "rend" "push_back" "push_front" "insert"
    "size" "max_size" "empty" "resize" "back" "front" "at"
    "erase" "clear" "pop_back" "pop_front"
    "swap" "assign"
    ;; -- string
    "npos" "c_str" "substr" "append" "replace" "find" "rfind" "find_first_of"
    "find_last_of" "find_first_not_of" "find_last_not_of" "compare"
    ;; -- bitset
    "any" "none" "count" "test" "set" "reset" "flip" "to_ulong"
    ;; sequencial container adaptor method
    ;; -- stack and queue and priority_queue
    "container_type"
    "pop" "top" "push"
    ;; associative container typedefs and methods
    "key_type" "mapped_type" "value_type" "count" "find"
    "lower_bound" "upper_bound" "equal_range"
    ;;
    ;;
    ;; * iterator
    "iterator" "const_iterator" "reverse_iterator" "const_reverse_iterator"
    "front_iterator" "back_iterator" "insert_iterator"
    "istream_iterator" "ostream_iterator"
    ;; iterator related types and members
    "difference_type" "base"
    ;;
    ;;
    ;; * standard library algorithm
    ;; header: <algorithm>
    ;;
    ;; algorithm summary
    ;; collected from appendix A of the book
    ;; Cpp Primer, 4th, english version, 2006
    ;; 1. algorithm to find an object
    ;; 1.1 simple find algorithm -- require: input iterators
    ;; find(beg, end, val)
    ;; find_if(beg, end, unaryPred)
    "find" "find_if"
    ;; count(beg, end, val)
    ;; count(beg, end.unaryPred)
    "count" "count_if"
    ;; 1.2 algorithm to find one of many values -- require: forward iterators
    ;; find_first_of(beg1, end1, beg2, end2)
    ;; find_first_of(beg1, end1, beg2, end2, binaryPred)
    "find_first_of"
    ;; find_end(beg1, end1, beg2, end2)
    ;; find_end(beg1, end1, beg2, end2, binaryPred)
    "find_end"
    ;; 1.3 algorithm to find a subsequence -- require: forward iterators
    ;; adjacent_find(beg, end)
    ;; adjacent_find(beg, end, binaryPred)
    "adjacent_find"
    ;; search(beg1, end1, beg2, end2)
    ;; search(beg1, end1, beg2, end2, binaryPred)
    "search"
    ;; search_n(beg1, end1, count, value)
    ;; search_n(beg1, end1, count, value, binaryPred)
    "search_n"
    ;; 2. other read-only algorithms
    ;;   -- require: input iterators for first two argument
    ;; for_each(beg, end, f)
    ;;   beg, end: input iterators
    ;;   f is function or function object
    "for_each"
    ;; mismatch(beg1, end1, beg2)
    ;; mismatch(beg1, end1, beg2, binaryPred)
    ;;   beg1, end1, beg2: input iterators
    "mismatch"
    ;; equal(beg1, end1, beg2)
    ;; equal(beg1, end1, beg2, binaryPred)
    ;;   beg1, end1, beg2: input iterators
    "equal"
    ;; 3. binary search algorithms  -- require: forward iterators
    ;; lower_bound(beg, end, val)
    ;; lower_bound(beg, end, val, comp)
    "lower_bound"
    ;; upper_bound(beg, end, val)
    ;; upper_bound(beg, end, val, comp)
    "upper_bound"
    ;; equal_range(beg, end, val)
    ;; equal_range(beg, end, val, comp)
    "equal_range"
    ;; binary_search(beg, end, val)
    ;; binary_search(beg, end, val, comp)
    "binary_search"
    ;; 4. algorithms that write container elements
    ;; 4.1 algorithms that write but do not read elements
    ;;   -- require: an output iterator that denotes a destination
    ;; fill_n(dest, cnt, val)
    ;; generate_n(dest, cnt, Gen)
    "fill_n" "generate_n"
    ;; 4.2 algorithms that write elements using input iterators
    ;;   -- require: the input range must be input iterators,
    ;;               dest to be an output iterator
    ;; copy(beg, end, dest)
    "copy"
    ;; transform(beg, end, dest, unaryOp)
    ;; transform(beg, end, beg2, dest, binaryOp)
    "transform"
    ;; replace_copy(beg, end, dest, old_val, new_val)
    ;; replace_coyp_if(beg, end, dest, unaryPred, new_val)
    "replace_copy" "replace_coyp_if"
    ;; merge(beg1, end1, beg2, end2, dest)
    ;; merge(beg1, end1, beg2, end2, dest, comp)
    ;;   both input sequences must be sorted.
    "merge"
    ;; 4.3 algorithms that write elements using forward iterators
    ;;   -- require: forward iterators
    ;; swap(elem1, elem2)
    "swap"
    ;; iter_swap(iter1, iter2)
    "iter_swap"
    ;; swap_ranges(beg1, end1, beg2)
    "swap_ranges"
    ;; fill(beg, end, val)
    ;; generate(beg, end, Gen)
    "fill" "generate"
    ;; replace(beg, end, old_val, new_val)
    ;; replace_if(beg, end, unaryPred, new_val)
    "replace" "replace_if"
    ;; 4.4. algorithms that write elements using bidirectional iterators
    ;;   -- require: bidirectional iterators
    ;; copy_backward(beg, end, dest)
    "copy_backward"
    ;; inplace_merge(beg, mid, end)
    ;; inplace_merge(beg, mid, end, comp)
    "inplace_merge"
    ;; 5. partitioning and sorting algorithms
    ;; 5.1 partitioning algorithms -- require: bidirectional iterators
    ;; stable_partition(beg, end, unaryPred)
    ;; partition(beg, end, unaryPred)
    "stable_partition" "partition"
    ;; 5.2 sorting algorithms  -- require: random-access iterators
    ;; sort(beg, end)
    ;; stable_sort(beg, end)
    ;; sort(beg, end, comp)
    ;; stable_sort(beg, end, comp)
    "sort" "stable_sort"
    ;; partial_sort(beg, mid, end)
    ;; partial_sort(beg, mid, end, comp)
    "partial_sort"
    ;; partial_sort_copy(beg, end, destBeg, destEnd)
    ;; partial_sort_copy(beg, end, destBeg, destEnd, comp)
    "partial_sort_copy"
    ;; nth_element(beg, nth, end)
    ;; nth_element(beg, htn, end, comp)
    "nth_element"
    ;; 6. General reordering operations
    ;; 6.1 reordering algorithms using forward iterators
    ;;   -- require: forward iterators
    ;; remove(beg, end, val)
    ;; remove_if(beg, end, unaryPred)
    "remove" "remove_if"
    ;; unique(beg, end)
    ;; unique(beg, end, binaryPred)
    "unique"
    ;; rotate(beg, mid, end)
    "rotate"
    ;; 6.2 reordering algorithms using bidirectional iterators
    ;;   -- require: bidirectional iterators
    ;; reverse(beg, end)
    ;; reverse_copy(beg, end, dest)
    "reverse" "reverse_copy"
    ;; 6.3 reordering algorithms writing to output iterators
    ;;   -- require: forward iterators for the input sequence
    ;;               an output iterator for the destination
    ;; remove_copy(beg, end, dest, val)
    ;; remove_copy_if(beg, end, dest, unaryPred)
    "remove_copy" "remove_copy_if"
    ;; unique_copy(beg, end, dest)
    ;; unique_copy(beg, end, dest, binaryPred)
    "unique_copy"
    ;; rotate_copy(beg, mid, end, dest)
    ;;   beg, mid, end: forward iterators
    ;;   dest: output iterators
    "rotate_copy"
    ;; 6.4 reordering algorithms using random-access iterators
    ;;   -- require: random-access iterators
    ;; random_shuffle(beg, end)
    ;; random_shuffle(beg, end, rand)
    "random_shuffle"
    ;; 7. permutation algorithms
    ;;   -- require: bidirectional iterators
    ;; next_permutation(beg, end)
    ;; next_permutation(beg, end, comp)
    "next_permutation"
    ;; prev_permutation(beg, end)
    ;; prev_permutation(beg, end, comp)
    "prev_permutation"
    ;; 8. Set algorithms for sorted sequences
    ;;   -- require: input iterators and output iterators except for includes
    ;; includes(beg, end, beg2, end2)
    ;; includes(beg, end, beg2, end2, comp)
    "includes"
    ;; set_union(beg, end, beg2, end2, dest)
    ;; set_union(beg, end, beg2, end2, dest, comp)
    "set_union"
    ;; set_intersection(beg, end, beg2, end2, dest)
    ;; set_intersection(beg, end, beg2, end2, dest, comp)
    "set_intersection"
    ;; set_difference(beg, end, beg2, end2, dest)
    ;; set_difference(beg, end, beg2, end2, dest, comp)
    "set_difference"
    ;; set_symmetric_difference(beg, end, beg2, end2, dest)
    ;; set_symmetric_difference(beg, end, beg2, end2, dest, comp)
    "set_symmetric_difference"
    ;; 9. minimum and maximum values
    ;; min(val1, val2)
    ;; min(val1, val2, comp)
    ;; max(val1, val2)
    ;; max(val1, val2, comp)
    ;;   operate on values rather than sequences
    "min" "max"
    ;; min_element(beg, end)
    ;; min_element(beg, end, comp)
    ;; max_element(beg, end)
    ;; max_element(beg, end, comp)
    "mix_element" "max_element"
    ;; 9.1 lexicographical comparison -- require: input iterators
    ;; lexicographical_compare(beg1, end1, beg2, end2)
    ;; lexicographical_compare(beg1, end1, beg2, end2, comp)
    "lexicographical_compare"
    ;; 10. numerc algorithms
    ;;   header: <numeric>
    ;;   -- requrie: input iterators, dest to be an output iterator
    ;; accumulate(beg, end, init)
    ;; accumulate(beg, end, init, BinaryOp)
    "accumulate"
    ;; inner_product(beg1, end1, beg2, init)
    ;; inner_product(beg1, end1, beg2, init, BinOp1, BinOp2)
    "inner_product"
    ;; partial_sum(beg, end, dest)
    ;; partial_sum(beg, end, dest, BinaryOp)
    "partial_sum"
    ;; adjacent_difference(beg, end, dest)
    ;; adjacent_difference(beg, end, dest, BinaryOp)
    "adjacent_difference"
    ;;
    ;; container-specific algorithm -- list
    "merge" "remove" "remove_if" "reverse" "sort" "splice" "unique"
    ;;
    ;; * library-defined function objects
    ;; header: <functional>
    ;; 1. arithmetic function objects types
    "plus" "minus" "multiplies" "divides" "modulus" "negate"
    ;; 2. relational function objects types
    "equal_to" "not_equal_to" "greater" "greater_equal" "less" "less_equal"
    ;; 3. logical function object types
    "logical_and" "logical_or" "logical_not"
    ;; * library-defined function adaptors for function objects
    ;; header: <functional>
    ;; 1. binder
    "bind1st" "bind2nd"
    ;; 2. negator
    "not1" "not2"
    ;;
    ;;
    ;; * standard library objects, members and methods
    ;; exception
    "exception" "runtime_error" "range_error" "overflow_error"
    "underflow_error"
    "logic_error" "domain_error" "invalid_argument"
    "length_error" "out_of_range"
    ;; methods
    "what"
    ;;
    ;; standard library io
    "cin" "cout" "cerr" "clog"
    "istream" "ostream" "iostream" "ofstream" "ifstream" "ostringstream"
    "istringstream" "stringstream" "fstream"
    "wcin" "wcout" "wcerr"
    "wistream" "wostream" "wiostream" "wofstream" "wifstream"
    "wostringstream" "wistringstream" "wstringstream" "wfstream"
    ;; io members and methods
    ;; -- [io]*stream
    "iostate" "badbit" "failbit" "eofbit" "eof" "fail" "bad" "good"
    "clear" "setstate" "rdstate"
    "tie"
    ;; -- [io]*fstream
    "open" "close"
    ;; [io]*stringstream
    "str"
    ;; io flags and manipulator
    "flush" "endl" "ends" "unitbuf" "nounitbuf"
    ;; file modes
    "in" "out" "app" "ate" "trunc" "binary"
    ;;
    ;; misc facility
    ;; header: <memory>
    "auto_ptr"
    ;; header: <new>
    "bad_alloc"
    ;; header: <typeinfo>
    "bad_cast"
    ;;
    ;; functions
    "getline"
    ;; macros and variables
    ;;
    ;; types, enums, structs and classes
    "wchar_t"
    ;;
    ;; struct/class members and methods
    ;; #include <stdio.h>
    "EOF"
    ))


(ac-define-source libcpp
  '((candidates . ac-libcpp-candidates)
    (symbol . "+")
    (cache)))


;;; define source for Boost library keywords

(defun ac-libboost-candidates ()
  "A keyword candidates produce function for Boost library keywords."
  '(;;
    ;; namespace
    "boost"
    ;; functions
    ;;
    ;; macros and variables
    ;;
    ;; types enums and structs
    ;;
    ;; struct members
    ))


(ac-define-source libboost
  '((candidates . ac-libboost-candidates)
    (symbol . "B")
    (cache)))


;;; define source for my personal c/c++ library keywords

(defun ac-liblw-candidates ()
  "A keyword candidates produce function for my peronsal c/c++ library."
  '(
    ;; #include <lw_share.h>
    "NUL"
    "cbool_tag" "cfalse" "ctrue"
    ;; #include <lw_assert.h>
    "ASSERT" "REQUIRE" "ENSURE" "INVARIANT" "ALLEGE"
    ))


(ac-define-source liblw
  '((candidates . ac-liblw-candidates)
    (symbol . "L")
    (cache)))


;;; define source for apue keywords

(defun ac-apue-candidates ()
  "A keyword candidates produce function for apue keywords."
  '(
    ;;
    ;;
    ;; * Chapter 1 UNIX System Overview
    ;; 1.7 Error Handling
    ;; #include <errno.h>
    ;; extern int errno;
    "errno"
    ;; #include <string.h>
    ;; char *strerror(int errnum);
    ;;     returns: pointer to message string
    "strerror"
    ;; #include <stdio.h>
    ;; void perror(const char *msg);
    "perror"
    ;;
    ;; * Chapter 2 UNIX Standardization and Implementations
    ;; 2.2 UNIX Standardization
    ;; 2.2.3 The Single UNIX Specification
    ;; Figure 2.5 POSIX.1 opiontal interface groups and codes
    "_POSIX_ADVISORY_INFO" "_POSIX_ASYNCHRONOUS_IO" "_POSIX_BARRIERS"
    "_POSIX_CPUTIME" "_POSIX_CLOCK_SELECTION" "_POSIX_FSYNC"
    "_POSIX_IPV6" "_POSIX_MAPPED_FILES" "_POSIX_MEMLOCK"
    "_POSIX_MEMLOCK_RANGE" "_POSIX_MONOTONIC_CLOCK"
    "_POSIX_MEMORY_PROTECTION" "_POSIX_MESSAGE_PASSING"
    "_POSIX_PRIORITIZED_IO" "_POSIX_PRIORITIZED_SCHEDULING"
    "_POSIX_RAW_SOCKETS" "_POSIX_REALTIME_SIGNALS"
    "_POSIX_SEMAPHORES" "_POSIX_SHARED_MEMORY_OBJECTS"
    "_POSIX_SYNCHRONIZED_IO" "_POSIX_SPIN_LOCKS" "_POSIX_SPAWN"
    "_POSIX_SPORADIC_SERVER" "_POSIX_THREAD_CPUTIME"
    "_POSIX_TRACE_EVENT_FILTER" "_POSIX_THREADS" "_POSIX_TIMEOUTS"
    "_POSIX_TIMERS" "_POSIX_THREAD_PRIO_INHERIT"
    "_POSIX_THREAD_PRIO_PROTECT" "_POSIX_THREAD_PRIORITY_SCHEDULING"
    "_POSIX_TRACE" "_POSIX_TRACE_INHERIT" "_POSIX_TRACE_LOG"
    "_POSIX_THREAD_ATTR_STACKADDR" "_POSIX_THREAD_SAFE_FUNCTIONS"
    "_POSIX_THREAD_PROCESS_SHARED" "_POSIX_THREAD_SPORADIC_SERVER"
    "_POSIX_THREAD_ATTR_STACKSIZE" "_POSIX_TYPED_MEMORY_OBJECTS"
    "_XOPEN_UNIX" "_XOPEN_STREAMS"
    ;; 2.5 Limits
    ;; #include <limits.h>
    "CHAR_BIT" "CHAR_MAX" "CHAR_MIN" "SCHAR_MAX" "SCHAR_MIN" "UCHAR_MAX"
    "INT_MAX" "INT_MIN" "UINT_MAX" "SHRT_MIN" "SHRT_MAX" "USHRT_MAX"
    "LONG_MIN" "LONG_MAX" "ULONG_MAX" "LLONG_MAX" "LLONG_MIN" "ULLONG_MAX"
    "MB_LEN_MAX"
    ;; 2.5.1 ISO C Limits
    ;; #include <stdio.h>
    ;; FOPEN_MAX STREAM_MAX TMP_MAX
    "FOPEN_MAX" "STREAM_MAX" "TMP_MAX"
    ;; 2.5.2 POSIX Limits
    ;; 1) Invariant minimum values: 19 constants
    "_POSIX_ARG_MAX" "_POSIX_CHILD_MAX" "_POSIX_HOST_NAME_MAX"
    "_POSIX_LINK_MAX" "_POSIX_LOGIN_NAME_MAX" "_POSIX_MAX_CANON"
    "_POSIX_MAX_INPUT" "_POSIX_NAME_MAX" "_POSIX_NGROUPS_MAX"
    "_POSIX_OPEN_MAX" "_POSIX_PATH_MAX" "_POSIX_PIPE_BUF"
    "_POSIX_RE_DUP_MAX" "_POSIX_SSIZE_MAX" "_POSIX_STREAM_MAX"
    "_POSIX_SYMLINK_MAX" "_POSIX_SYMLOOP_MAX" "_POSIX_TTY_NAME_MAX"
    "_POSIX_TZNAME_MAX"
    ;; 2) Invariant value
    "SSIZE_MAX"
    ;; 3) Runtime increasable values
    "CHARCLASS_NAME_MAX" "COLL_WEIGHTS_MAX" "LINE_MAX"
    "NGROUPS_MAX" "RE_DUP_MAX"
    ;; 4) Runtime Invariant values, possibly indeterminate
    "ARG_MAX" "CHILD_MAX" "HOST_NAME_MAX" "LOGIN_NAME_MAX" "OPEN_MAX"
    "PAGESIZE" "RE_DUP_MAX" "STREAM_MAX" "SYMLOOP_MAX" "TTY_NAME_MAX"
    "TZNAME_MAX"
    ;; 5) Pathname variable values, possibly indeterminate
    "FILESIZEBITS" "LINK_MAX" "MAX_CANON" "MAX_INPUT"
    "NAME_MAX" "PATH_MAX" "PIPE_BUF" "SYMLINK_MAX"
    ;; 2.5.3 XSI Limits
    ;; 1) Invariant minimum values
    "NL_ARGMAX" "NL_LANGMAX" "NL_MSGMAX" "NL_NMAX" "NL_SETMAX" "NL_TEXTMAX"
    "NZERO" "_XOPEN_IOV_MAX" "_XOPEN_NAME_MAX" "_XOPEN_PATH_MAX"
    ;; 2) Numerical limits
    "LONG_BIT" "WORD_BIT"
    ;; 3) Runtime invariant values, possibly indeterminate
    "ATEXIT_MAX" "IOV_MAX" "PAGE_SIZE"
    ;; 2.5.4 sysconf, pathconf, and fpathconf Functions
    ;; #include <unistd.h>
    ;; long sysconf(int name);
    ;; long pathconf(const char *pathname, int name);
    ;; long fpathconf(int filedes, int name);
    ;;     all three return: corresponding value if OK, -1 on error(or indeterminate)
    "sysconf" "pathconf" "fpathconf"
    ;; name argument to sysconf()
    "_SC_ARG_MAX" "_SC_ATEXIT_MAX" "_SC_CHILD_MAX" "_SC_CLK_TCK"
    "_SC_COLL_WEIGHTS_MAX" "_SC_HOST_NAME_MAX" "_SC_IOV_MAX" "_SC_LINE_MAX"
    "_SC_LOGIN_NAME_MAX" "_SC_NGROUPS_MAX" "_SC_OPEN_MAX" "_SC_PAGESIZE"
    "_SC_PAGE_SIZE" "_SC_RE_DUP_MAX" "_SC_STREAM_MAX" "_SC_SYMLOOP_MAX"
    "_SC_TTY_NAME_MAX" "_SC_TZNAME_MAX"
    ;; name argument to pathconf() and fpathconf()
    "_PC_FILESIZEBITS" "_PC_LINK_MAX" "_PC_MAX_CANON" "_PC_MAX_INPUT"
    "_PC_NAME_MAX" "_PC_PATH_MAX" "_PC_PIPE_BUF" "_PC_SYMLINK_MAX"
    ;; 2.6 Options
    ;; Figure 2.17 Options and name arguments to sysconf
    "_POSIX_JOB_CONTROL" "_SC_JOB_CONTROL"
    "_POSIX_READRE_WRITER_LOCKS" "_SC_READER_WRITER_LOCKS"
    "_POSIX_SAVED_IDS" "_SC_SAVED_IDS"
    "_POSIX_SHELL" "_SC_SHELL"
    "_POSIX_VERSION" "_SC_VERSION"
    "_XOPEN_CRYPT" "_SC_XOPEN_CRYPT"
    "_XOPEN_LEGACY" "_SC_XOPEN_LEGACY"
    "_XOPEN_REALTIME" "_SC_XOPEN_REALTIME"
    "_XOPEN_REALTIME_THREADS" "_SC_XOPEN_REALTIME_THREADS"
    "_XOPEN_VERSION" "_SC_XOPEN_VERSION"
    ;; Figure 2.18 Options and name arguments to pathconf and fpathconf
    "_POSIX_CHOWN_RESTRICTED" "_PC_CHOWN_RESTRICTED"
    "_POSIX_NO_TRUNC" "_PC_NO_TRUNC"
    "_POSIX_VDISABLE" "_PC_VDISABLE"
    "_POSIX_ASYNC_IO" "_PC_ASYNC_IO"
    "_POSIX_PRIO_IO" "_PC_PRIO_IO"
    "_POSIX_SYNC_IO" "_PC_SYNC_IO"
    ;; 2.7 Feature Test Macros
    "_POSIX_SOURCE"    ;; superseded by _POSIX_C_SOURCE contant of POSIX.1 2001
    "_POSIX_C_SOURCE"
    "_XOPEN_SOURCE"
    "__STDC__"
    ;; 2.8 Primitive System Data Types
    ;; Figure 2.20 Some common primitive system data types
    "caddr_t" "clock_t" "comp_t" "dev_t" "fd_set" "fpos_t" "gid_t" "ino_t"
    "mode_t" "nlink_t" "off_t" "pid_t" "ptrdiff_t" "rlim_t" "sig_atomic_t"
    "sigset_t" "size_t" "ssize_t" "time_t" "uid_t" "wchar_t"
    ;;
    ;; * Chapter 3 File I/O
    ;; 3.2 File Descriptors
    ;; #include <unistd.h>
    "STDIN_FILENO" "STDOUT_FILENO" "STDERR_FILENO"
    ;; 3.3 open Function
    ;; #include <fcntl.h>
    ;; int open(const char *pathname, int oflag, ... /* mode_t mode */ );
    ;;     returns: file descriptor if OK, -1 on error
    "open"
    ;; oflag options:
    "O_RDONLY" "O_WRONLY" "ORDWR"
    ;; optional constants
    "O_APPEND" "O_CREAT" "O_EXCL" "O_TRUNC" "O_NOCTTY" "O_NONBLOCK"
    ;; optional constants of synchronized input and output options
    "O_DSYNC" "O_RSYNC" "O_SYNC"
    ;; 3.4 creat Function
    ;; #include <fcntl.h>
    ;; int creat(const char *pathname, mode_t mode);
    ;;     returns: file descriptor opened for write-only if OK, -1 on error
    "creat"
    ;; 3.5 close Function
    ;; #include <unistd.h>
    ;; int close(int filedes);
    "close"
    ;; 3.6 lseek Function
    ;; #include <unistd.h>
    ;; off_t lseek(int filedes, off_t offset, int whence);
    ;;     returns: new file offset if OK, -1 on error
    "lseek"
    ;; whence argument:
    "SEEK_SET" "SEEK_CUR" "SEEK_END"
    ;; Figure 3.3 Data size options and name arguments to sysconf
    "_POSIX_V6_ILP32_OFF32" "_SC_V6_ILP32_OFF32"
    "_POSIX_V6_ILP32_OFFBIG" "_SC_V6_ILP32_OFFBIG"
    "_POSIX_V6_LP64_OFF64" "_SC_V6_LP64_OFF64"
    "_POSIX_V6_LP64_OFFBIG" "_SC_V6_LP64_OFFBIG"
    "_FILE_OFFSET_BITS"
    ;; 3.7 read Function
    ;; #include <unistd.h>
    ;; ssize_t read(int filedes, void *buf, size_t nbytes);
    ;;     returns: number of bytes read, 0 if end of file, -1 on error
    "read"
    ;; 3.8 write Function
    ;; #include <unistd.h>
    ;; ssize_t write(int filedes, const void *buf, size_t nbytes);
    ;;     returns: number of bytes written if OK, -1 on error
    "write"
    ;; 3.11 Atomic Operations
    ;; pread and pwrite Functions
    ;; #include <unistd.h>
    ;; ssize_t pread(int filedes, void *buf, size_t nbytes, off_t offset);
    ;;     returns: number of bytes read, 0 if end of file, -1 on error
    ;; ssize_t pwrite(int filedes, const void *buf, size_t nbytes, off_t offset);
    ;;     returns: number of bytes written if OK, -1 on error
    "pread" "pwrite"
    ;; 3.12 dup and dup2 Functions
    ;; #include <unistd.h>
    ;; int dup(int filedes);
    ;; int dup2(int filedes, int filedes2);
    ;;     both returns: new file descriptor if OK, -1 on error
    "dup" "dup2"
    ;; 3.13 sync, fsync, and fdatasync Functions
    ;; #include <unistd.h>
    ;; int fsync(int filedes);
    ;; int fdatasync(int filedes);
    ;;     returns: 0 if OK, -1 on error
    ;; void sync(void);
    "fsync" "fdatasync" "sync"
    ;; 3.14 fcntl Function
    ;; #include <fcntl.h>
    ;; int fcntl(int filedes, int cmd, ... /* int arg */ );
    ;;     returns: depends on cmd if OK(see following), -1 on error
    "fcntl"
    ;; cmd arguement:
    "F_DUPFD" "F_GETFD" "F_SETFD" "F_GETFL" "F_SETFL" "F_GETOWN" "F_SETOWN"
    ;; to get three access-mode flags - O_RDONLY, O_WRONLY, O_RDWR
    "FD_CLOEXEC" "O_ACCMODE"
    ;; 3.15 ioctl Function
    ;; #include <unistd.h>     /* System V */
    ;; #include <sys/ioctl.h>  /* BSD and Linux */
    ;; #include <stropts.h>    /* XSI STREAMS */
    ;; int ioctl(int filedes, int request, ...);
    ;;     returns: -1 on error, something else if OK
    "ioctl"
    ;;
    ;; * Chapter 4 Files and Directories
    ;; 4.2 stat, fstat, and lstat Functions
    ;; #include <sys/stat.h>
    ;; int stat(const char *restrict pathname, struct stat *restrict buf);
    ;; int fstat(int filedes, struct stat *buf);
    ;; int lstat(const char *restrict pathname, struct stat *restrict buf);
    ;;     all three return: 0 if OK, -1 on error
    "stat" "fstat" "lstat"
    ;; struct stat {
    ;;     mode_t     st_mode;     /* file type & mode (permissions) */
    ;;     ino_t      st_ino;      /* i-node number (serial number) */
    ;;     dev_t      st_dev;      /* device number (file system) */
    ;;     dev_t      st_rdev;     /* device number for special files */
    ;;     nlink_t    st_nlink;    /* number of links */
    ;;     uid_t      st_uid;      /* user ID of owner */
    ;;     gid_t      st_gid;      /* group ID of owner */
    ;;     off_t      st_size;     /* size in bytes, for regular files */
    ;;     time_t     st_atime;    /* time of last access */
    ;;     time_t     st_mtime;    /* time of last modification */
    ;;     time_t     st_ctime;    /* time of last file status change */
    ;;     blksize_t  st_blksize;  /* best I/O block size */
    ;;     blkcnt_t   st_blocks;   /* number of disk blocks allocated */
    ;; };
    "stat"
    "st_mode" "st_ino" "st_dev" "st_rdev" "st_nlink" "st_uid" "st_gid"
    "st_size" "st_atime" "st_mtime" "st_ctime" "st_blksize" "st_blocks"
    ;; Figure 4.1 File type macros in <sys/stat.h>
    ;;   MACRO          Type of file
    ;;   S_ISREG()      regular file
    ;;   S_ISDIR()      directory file
    ;;   S_ISCHR()      character special file
    ;;   S_ISBLK()      block special file
    ;;   S_ISFIFO()     pipe or FIFO
    ;;   S_ISLNK()      symbolic link
    ;;   S_ISSOCK()     socket (must define _GNU_SOURCE on Linux)
    ;; argument: st_mode member of stat structure
    "S_ISREG" "S_ISDIR" "S_ISCHR" "S_ISBLK" "S_ISFIFO" "S_ISLNK" "S_ISSOCK"
    ;; Figure 4.2 IPC type macros in <sys/stat.h>
    ;;   MACRO          Type of object
    ;;   S_TYPEISMQ()   message queue
    ;;   S_TYPEISSEM()  semaphore
    ;;   S_TYPEISSHM()  shared memory object
    ;; argument: a pointer to stat structure
    "S_TYPEISMQ" "S_TYPEISSEM" "S_TYPEISSHM"
    ;; historical note on S_ISxxx macros
    ;; "S_IFMT"  ;; obselete
    ;; 4.4 Set-User-ID and Set-Group-ID
    "S_ISUID" "S_ISGID"
    ;; 4.5 File Access Permissions
    ;; Figure 4.6 The nine file access permission bits, from <sys/stat.h>
    ;;   st_mode mask   Meaning
    ;;   S_IRUSR        user-read
    ;;   S_IWUSR        user-write
    ;;   S_IXUSR        user-execute
    ;;   S_IRGRP        group-read
    ;;   S_IWGRP        group-write
    ;;   S_IXGRP        group-execute
    ;;   S_IROTH        other-read
    ;;   S_IWOTH        other-write
    ;;   S_IXOTH        other-execute
    "S_IRUSR" "S_IWUSR" "S_IXUSR"
    "S_IRGRP" "S_IRGRP" "S_IRGRP"
    "S_IROTH" "S_IWOTH" "S_IXOTH"
    ;; 4.7 access Function
    ;; #include <unistd.h>
    ;; int access(const char *pathname, int mode);
    ;;     returns: 0 if OK, -1 on error
    "access"
    ;; Figure 4.7 The mode constants for access function, from <unistd.h>
    ;;   mode      Description
    ;;   R_OK      test for read permission
    ;;   W_OK      test for write permission
    ;;   X_OK      test for execute permission
    ;;   F_OK      test for existence of file
    "R_OK" "W_OK" "X_OK" "F_OK"
    ;; 4.9 chmod and fchmod Functions
    ;; #include <sys/stat.h>
    ;; int chmod(const char *pathname, mode_t mode);
    ;; int fchmod(int filedes, mode_t mode);
    ;;     both return: 0 if OK, -1 on error
    "chmod" "fchmod"
    ;; Figure 4.11 The mode constants for chmod functions, from <sys/stat.h>
    ;;   mode         Description
    ;;   S_ISUID      set-user-ID on execution
    ;;   S_ISGID      set-group-ID on execution
    ;;   S_ISVTX      saved-text(sticky bit)
    ;;   S_IRWXU      read, write, and execute by user(owner)
    ;;     S_IRUSR      read by user(owner)
    ;;     S_IWUSR      write by user(owner)
    ;;     S_IXUSR      execute by user(owner)
    ;;   S_IRWXG      read, write, and execute by group
    ;;     S_IRGRP      read by group
    ;;     S_IWGRP      write by group
    ;;     S_IXGRP      execute by group
    ;;   S_IRWXO      read, write, and execute by other(world)
    ;;     S_IROTH      read by other(world)
    ;;     S_IWOTH      write by other(world)
    ;;     S_IXOTH      execute by other(world)
    "S_ISUID" "S_ISGID" "S_ISVTX" "S_IRWXU" "S_IRWXG" "S_IRWXO"
    ;; 4.11 chown, fchown, and lchown
    ;; #include <unistd.h>
    ;; int chown(const char *pathname, uid_t owner, gid_t group);
    ;; int fchown(int filedes, uid_t owner, gid_t group);
    ;; int lchown(const char *pathname, uid_t owner, gid_t group);
    ;;     all three return: 0 if OK, -1 on error
    "chown" "fchown" "lchown"
    ;; 4.13 File Truncation
    ;; #include <unistd.h>
    ;; int truncate(const char *pathname, off_t length);
    ;; int ftruncate(int filedes, off_t length);
    ;;     both return: 0 if OK, -1 on error
    "truncate" "ftruncate"
    ;; 4.15 link, unlink, remove, and rename Functions
    ;; #include <unistd.h>
    ;; int link(const char *existingpath, const char *newpath);
    ;;     returns: 0 if OK, -1 on error
    ;; #include <unistd.h>
    ;; int unlink(const char *pathname);
    ;;     returns: 0 if OK, -1 on error
    "link" "unlink"
    ;; #include <stdio.h>
    ;; int remove(const char *pathname);
    ;;     returns: 0 if OK, -1 on error
    "remove"
    ;; #include <stdio.h>
    ;; int rename(const char *oldname, const char *newname);
    ;;     returns: 0 if OK, -1 on error
    "rename"
    ;; 4.17 symlink and readlink Functions
    ;; #include <unistd.h>
    ;; int symlink(const char *actualpath, const char *sympath);
    ;;     returns: 0 if OK, -1 on error
    ;; #include <unistd.h>
    ;; ssize_t readlink(const char *restrict pathname, char *restrict buf,
    ;;                  size_t bufsize);
    ;;     returns: number of bytes read if OK, -1 on error
    "symlink" "readlink"
    ;; 4.19 utime Function
    ;; #include <utime.h>
    ;; int utime(const char *pathname, const struct utimbuf *times);
    ;;     returns: 0 if OK, -1 on error
    "utime"
    ;; struct utimbuf {
    ;;     time_t  actime;   /* access time */
    ;;     time_t  modtime;  /* modification time */
    ;; };
    "utimbuf" "actime" "modtime"
    ;; 4.20 mkdir and rmdir Functions
    ;; #include <sys/stat.h>
    ;; int mkdir(const char *pathname, mode_t mode);
    ;;     returns: 0 if OK, -1 on error
    "mkdir"
    ;; #include <unistd.h>
    ;; int rmdir(const char *pathname);
    ;;     reutrns: 0 if OK, -1 on error
    "rmdir"
    ;; 4.21 Reading Directories
    ;; #include <dirent.h>
    ;; DIR *opendir(const char *pathname);
    ;;     returns: pointer if OK, NULL on error
    "opendir"
    ;; struct dirent *readdir(DIR *dp);
    ;;     returns: pointer if OK, NULL at end of directory or error
    "readdir"
    ;; void rewinddir(DIR *dp);
    ;; int closedir(DIR *dp);
    ;;     returns: 0 if OK, -1 on error
    "rewinddir"
    "closedir"
    ;; long telldir(DIR *dp);
    ;;     returns: current location in direcotry associated with dp
    "telldir"
    ;; void seekdir(DIR *dp, long loc);
    ;;     returns: void
    "seekdir"
    ;; struct dirent {
    ;;     ino_t   d_ino;                  /* i-node number */
    ;;     char    d_name[NAME_MAX + 1];   /* null-terminaled filename */
    ;; }
    "dirent" "d_ino" "d_name"
    "DIR"
    ;; 4.22 chdir, fchdir, and getcwd Functions
    ;; #include <unistd.h>
    ;; int chdir(const char *pathname);
    ;; int fchdir(int filedes);
    ;;     both returns: 0 if OK, -1 on error
    "chdir" "fchdir"
    ;; #include <unistd.h>
    ;; char *getcwd(char *buf, size_t size);
    ;;     returns: buf if OK, NULL on error
    "getcwd"
    ;; 4.23 Device Special Files
    ;; BSD-based system: <sys/types.h>
    ;; Solaris: <sys/mkdev.h>
    ;; Linux: <sys/sysmacros.h> (included in <sys/types.h>)
    "major" "minor"
    ;;
    ;; * Chapter 5 Standard I/O Library
    ;; 5.2 Streams and FILE Objects
    "FILE"
    ;; #include <stdio.h>
    ;; #include <wchar.h>
    ;; int fwide(FILE *fp, int mode);
    ;;     returns: positive if stream is wide-oriented,
    ;;              negative if stream is byte-oriented,
    ;;              or 0 if stream has no orientation.
    "fwide"
    ;; 5.3 Standard Input, Standard Output, and Standard Error
    ;; #include <stdio.h>
    "stdin" "stdout" "stderr"
    ;; 5.4 Buffering
    ;; #include <stdio.h>
    ;; void setbuf(FILE *restrict fp, char *restrict buf);
    ;; int setvbuf(FILE *restrict fp, char *restrict buf, int mode);
    ;;     returns: 0 if OK, nonzero on error
    "setbuf" "setvbuf"
    ;; #include <stdio.h>
    ;; BUFSIZ
    "BUFSIZ"
    ;; mode argument of setvbuf():
    "_IOFBF" "_IOLBF" "_IONBF"
    ;; #include <stdio.h>
    ;; int fflush(FILE *fp);
    ;;     returns: 0 if OK, EOF on error
    "fflush"
    ;; 5.5 Opening a Streams
    ;; #include <stdio.h>
    ;; FILE *fopen(const char *restrict pathname, const char *restrict type);
    ;; FILE *freopen(const char *restrict pathname, const char *restrict type,
    ;;               FILE *restrict fp);
    ;; FILE *fdopen(int filedes, const char *type);
    ;;     all three return: file pointer if OK, NULL on error
    "fopen" "freopen" "fdopen"
    ;; type argument of these three functions:
    ;; Figure 5.2 The type argument for opening a standard I/O stream
    ;;   Type               Description
    ;;   r or rb            open for reading
    ;;   w or wb            truncate to 0 length or create for writing
    ;;   a or ab            append; open for writing at end of file,
    ;;                        or create for writing
    ;;   r+ or r+b or rb+   open for reading or writing
    ;;   w+ or w+b or wb+   truncate to 0 length or create for reading and
    ;;                        writing
    ;;   a+ or a+b or ab+   open or create for reading and writing at end
    ;;                        of file
    ;; #include <stdio.h>
    ;; int fclose(FILE *fp);
    ;;     returns: 0 if OK, EOF on error
    "fclose"
    ;; 5.6 Reading and Writing a Stream
    ;; Input Functions
    ;; #include <stdio.h>
    ;; int getc(FILE *fp);
    ;; int fgetc(FILE *fp);
    ;; int getchar(void);
    ;;     all three return: next character if OK, EOF on end of file or error
    "getc" "fgetc" "getchar"
    ;; #include <stdio.h>
    ;; int ferror(FILE *fp);
    ;; int feof(FILE *fp);
    ;;     both return: nonzero (true) if condition is true, 0 (false) otherwise
    ;; void clearerr(FILE *fp);
    "ferror" "feof" "clearerr"
    ;; #include <stdio.h>
    ;; int ungetc(int c, FILE *fp);
    ;;     returns: c if OK, EOF on error
    "ungetc"
    ;; Output Functions
    ;; #include <stdio.h>
    ;; int putc(int c, FILE *fp);
    ;; int fputc(int c, FILE *fp);
    ;; int putchar(int c);
    ;;     alll three return: c if OK, EOF on error
    "putc" "fputc" "putchar"
    ;; 5.7 Line-at-a-Time I/O
    ;; #include <stdio.h>
    ;; char *fgets(char *restrict buf, int n, FILE *restrict fp);
    ;; char *gets(char *buf);
    ;;     both return: buf if OK, NULL on end of file or error
    "fgets" "gets"
    ;; #include <stdio.h>
    ;; int fputs(const char *restrict str, FILE *restrict fp);
    ;; int puts(const char *str);
    ;;     both return: non-negative value if OK, EOF on error
    "fputs" "puts"
    ;; #include <stdio.h>
    ;; size_t fread(void *restrict ptr, size_t size, size_t nobj,
    ;;              FILE *restrict fp);
    ;; size_t fwrite(const void *restrict ptr, size_t size, size_t nobj,
    ;;               FILE *restrict fp);
    ;;     both return: number of objects read or written
    "fread" "fwrite"
    ;; #include <stdio.h>
    ;; long ftell(FILE *fp);
    ;;     returns: current file position indicator if OK, -1L on error
    ;; int fseek(FILE *fp, long offset, int whence);
    ;;     returns: 0 if OK, nonzero on error
    ;; void rewind(FILE *fp);
    "ftell" "fseek" "rewind"
    ;; #include <stdio.h>
    ;; off_t ftello(FILE *fp);
    ;;     returns: current file position indicator if OK, (off_t)-1 on error
    ;; int fseeko(FILE *fp, off_t offset, int whence);
    ;;     returns: 0 if OK, nonzero on error
    "ftello" "fseeko"
    ;; #include <stdio.h>
    ;; int fgetpos(FILE *restrict fp, fpos_t *restrict pos);
    ;; int fsetpos(FILE *fp, const fpos_t *pos);
    ;;     both return: 0 if OK, nonzero on error
    "fgetpos" "fsetpos"
    ;; 5.11 Formatted I/O
    ;; Formatted Output
    ;; #include <stdio.h>
    ;; int printf(const char *restrict format, ...);
    ;; int fprintf(FILE *restrict fp, const char *restrict format, ...);
    ;;     both return: number of character output if OK,
    ;;                  negative value if output error
    ;; int sprintf(char *restrict buf, const char *restrict format, ...);
    ;; int snprintf(char *restrict buf, size_t n,
    ;;              const char *restrict format, ...);
    "printf" "fprintf" "sprintf" "snprintf"
    ;; #include <stdarg.h>
    ;; #include <stdio.h>
    ;; int vprintf(const char *restrict format, va_list arg);
    ;; int vfprintf(FILE *restrict fp, const char *restrict format,
    ;;              va_list arg);
    ;;     both return: number of characters output if OK,
    ;;                  negative value if output error
    ;; int vsprintf(char *restrict buf, const char *restrict format,
    ;;              va_list arg);
    ;; int vsnprintf(char *restrict buf, size_t n,
    ;;               const char *restrict format, va_list arg);
    ;;     both return: number of characters stored in array if OK,
    ;;                  negative value if encoding error
    "vprintf" "vfprintf" "vsprintf" "vsnprintf"
    ;; Formatted Input
    ;; #include <stdio.h>
    ;; int scanf(const char *restrict format, ...);
    ;; int fscanf(FILE *restrict fp, const char *restrict format, ...);
    ;; int sscanf(const char *restrict buf, const char *restrict format,
    ;;            ...);
    ;;     all three return: number of input items assigned, EOF if input
    ;;                       error or end of file before any conversion
    "scanf" "fscanf" "sscanf"
    ;; #include <stdarg.h>
    ;; #include <stdio.h>
    ;; int vscanf(const char *restrict format, va_list arg);
    ;; int vfscanf(FILE *restrict fp, const char *restrict format,
    ;;             va_list arg);
    ;; int vsscanf(const char *restrict buf, const char *restrict format,
    ;;             va_list arg);
    ;;     all three return: number of input items assigned, EOF if input
    ;;                       error or end of file before any conversion
    "vscanf" "vfscanf" "vsscanf"
    ;; 5.12 Implementation Details
    ;; #include <stdio.h>
    ;; int fileno(FILE *fp);
    ;;     returns: the file descriptor associated with the stream
    "fileno"
    ;; 5.13 Temporary Files
    ;; #include <stdio.h>
    ;; char *tmpname(char *ptr);
    ;;     returns: pointer to unique pathname
    ;; FILE *tmpfile(void);
    ;;     returns: file pointer if OK, NULL on error
    "tmpnam" "tmpfile"
    ;; #include <stdio.h>
    "L_tmpnam"
    ;; #include <stdio.h>
    ;; char *tempnam(const char *direcotry, const char *prefix);
    ;;     returns: pointer to unique pathname
    "tempnam"
    "P_tmpdir"
    ;; #include <stdlib.h>
    ;; int mkstemp(char *template);
    ;;     returns: file descriptor if OK, -1 on error
    "mkstemp"
    ;; #include <stdlib.h>
    ;; char *mktemp(char *template);
    ;;     returns: always template. return template with the last six bytes
    ;;              modified to be a unique name on success. Or template is
    ;;              made an empty c string on fail.
    "mktemp" ;; legacy interface
    ;;
    ;; * Chapter 6 System Data Files and Infomation
    ;; 6.2 Password File
    ;; Figure 6.1 Fields in /etc/passwd file
    ;; #include <pwd.h>
    ;; struct passwd {
    ;;     char *pw_name;          /* user name */
    ;;     char *pw_passwd;        /* encrypted password */
    ;;     uid_t pw_uid;           /* numerical user ID */
    ;;     gid_t pw_gid;           /* numerical group ID */
    ;;     char *pw_gecos;         /* comment field */
    ;;     char *pw_dir;           /* initial working directory */
    ;;     char *pw_shell;         /* initial shell(user program) */
    ;;     /* The following members are not supported on Linux
    ;;      * but on BSD-derived platform */
    ;;     char *pw_class;         /* user access class */
    ;;     time_t pw_change;       /* next time to change password */
    ;;     time_t pw_expire;       /* account expiration time */
    ;; }
    "passwd"
    "pw_name" "pw_passwd" "pw_uid" "pw_gid" "pw_gecos" "pw_dir" "pw_shell"
    "pw_class" "pw_change" "pw_expire"
    ;; #include <pwd.h>
    ;; struct passwd *getpwuid(uid_t uid);
    ;; struct passwd *getpwnam(const char *name);
    ;;     both return: pointer if OK, NULL on error
    "getpwuid" "getpwnam"
    ;; #include <pwd.h>
    ;; struct passwd *getpwent(void);
    ;;     returns: pointer if OK, NULL on error or end of file
    ;; void setpwent(void);
    ;; void endpwent(void);
    "getpwent" "setpwent" "endpwent"
    ;; 6.3 Shadow Passwords
    ;; Figure 6.3 Fields in /etc/shadow file
    ;; #include <shadow.h>
    ;; struct spwd {
    ;;     char *sp_namp;        /* user login name */
    ;;     char *sp_pwdp;        /* encrypted password */
    ;;     int sp_lstchg;        /* days since Epoch of last password change */
    ;;     int sp_min;           /* days until change allowed */
    ;;     int sp_max;           /* days befoer change required */
    ;;     int sp_warn;          /* days warning for expiration */
    ;;     int sp_inact;         /* days before account inactive */
    ;;     int sp_expire;        /* days since Epoch when account expires */
    ;;     unsigned int sp_flag; /* reserved */
    ;; } /* int is defined as long int on Linux */
    "spwd"
    "sp_namp" "sp_pwdp" "sp_lstchg" "sp_min" "sp_max" "sp_warn" "sp_inact"
    "sp_expire" "sp_flag"
    ;; #include <shadow.h>
    ;; struct spwd *getspnam(const char *name);
    ;; struct spwd *getspent(void);
    ;;     both return: pointer if OK, NULL on error
    ;; void setspent(void);
    ;; void endspent(void);
    "getspnam" "getspent" "setspent" "endspent"
    ;; 6.4 Group File
    ;; Figure 6.4 Fields in /etc/group file
    ;; #include <grp.h>
    ;; struct group {
    ;;     char *gr_name;      /* group name */
    ;;     char *gr_passwd;    /* encrypted password */
    ;;     int gr_gid;         /* numerical group ID */
    ;;     char **gr_mem;      /* array of pointers to individual user names */
    ;; }
    "group" "gr_name" "gr_passwd" "gr_gid" "gr_mem"
    ;; #include <grp.h>
    ;; struct group *getgrgid(gid_t gid);
    ;; struct group *getgrnam(const char *name);
    ;;     both return: pointer if OK, NULL on error
    "getgrgid" "getgrnam"
    ;; #include <grp.h>
    ;; struct group *getgrent(void);
    ;;     returns: pointer if OK, NULL on error or end of file
    ;; void setgrent(void);
    ;; void endgrent(void);
    "getgrent" "setgrent" "endgrent"
    ;; 6.5 Supplementary Group IDs
    ;; #include <unistd.h>
    ;; int getgroups(int gidsetsize, gid_t grouplist[]);
    ;;     returns: number of supplementary group IDs if OK, -1 on error
    ;; #include <grp.h>     /* on Linux */
    ;; #include <unistd.h>  /* on FreeBSD, Mac OS X, and Solaris */
    ;; int setgroups(int ngroups, const gid_t grouplist[]);
    ;; #include <grp.h>     /* on Linux and Solaris */
    ;; #include <unistd.h>  /* on FreeBSD and Mac OS X */
    ;; int initgroups(const char *username, gid_t basegid);
    ;;     both return: 0 if OK, -1 on error
    "getgroups" "setgroups" "initgroups"
    ;; 6.8 Login Accounting
    ;; #include <utmp.h>
    ;; struct utmp {
    ;;     char ut_line[8];   /* tty line: "ttyh0", "ttyd0", "ttyp0", ... */
    ;;     char ut_name[8];   /* login name */
    ;;     long ut_time;      /* seconds since Epoch */
    ;; }
    "utmp"
    "ut_line" "ut_name" "ut_time"
    ;; 6.9 System Identification
    ;; #include <sys/utsname.h>
    ;; int uname(struct utsname *name);
    ;;     returns: non-negative value if OK, -1 on error
    "uname"
    ;; struct utsname {
    ;;     char sysname[];     /* name of the operating system */
    ;;     char nodename[];    /* name of this node */
    ;;     char release[];     /* current release of operating system */
    ;;     char version[];     /* current version of this release */
    ;;     char machine[];     /* name of hardware type */
    ;; }
    "utsname" "sysname" "nodename" "release" "version" "machine"
    ;; #include <unistd.h>
    ;; int gethostname(char *name, int namelen);
    ;;     returns: 0 if OK, -1 on error
    "gethostname"
    ;; 6.10 Time and Date Routines
    ;; #include <time.h>
    ;; time_t time(time_t *calptr);
    ;;     returns: value of time if OK, -1 on error
    "time"
    ;; #include <sys/time.h>
    ;; int gettimeofday(struct timeval *restrict tp, void *restrict tzp);
    ;;     return: 0 always
    "gettimeofday"
    ;; struct timeval {
    ;;     time_t tv_sec;    /* seconds */
    ;;     long   tv_usec;   /* microseconds */
    ;; };
    "timeval"
    "tv_sec" "tv_usec"
    ;; struct tm {          /* a broken-down time */
    ;;     int tm_sec;      /* seconds after the minute: [0 - 60] */
    ;;     int tm_min;      /* minutes after the hour: [0 - 59] */
    ;;     int tm_hour;     /* hours after midnight: [0 - 23] */
    ;;     int tm_mday;     /* day of the month: [1 - 31] */
    ;;     int tm_mon;      /* months since January: [0 - 11] */
    ;;     int tm_year;     /* years since 1900 */
    ;;     int tm_wday;     /* days since Sunday: [0 - 6] */
    ;;     int tm_yday;     /* days since January 1: [0 - 365] */
    ;;     int tm_isdst;    /* daylight saving time flag: <0, 0, >0 */
    ;; };
    "tm"
    "tm_sec" "tm_min" "tm_hour" "tm_mday" "tm_mon" "tm_year"
    "tm_wday" "tm_yday" "tm_isdst"
    ;; #include <time.h>
    ;; struct tm *gmtime(const time_t *calptr);
    ;; struct tm *localtime(const time_t *calptr);
    ;;     both return: pointer to broken-down time
    "gmtime" "localtime"
    ;; #include <time.h>
    ;; time_t mktime(struct tm *tmptr);
    ;;     returns: calendar time if OK, -1 on error
    "mktime"
    ;; #include <time.h>
    ;; char *asctime(const struct tm *tmptr);
    ;; char *ctime(const time_t *calptr);
    ;;     both return: pointer to null-terminaled string
    "asctime" "ctime"
    ;; #include <time.h>
    ;; size_t strftime(char *restrict buf, size_t maxsize,
    ;;                 const char *restrict format,
    ;;                 const struct tm *restrict tmptr);
    ;;     returns: number of characters stored in array if room, 0 otherwise
    "strftime"
    ;;
    ;; * Chapter 7 Process Enviroment
    ;; 7.3 Process Termination
    ;; Exit Functions
    ;; #include <stdlib.h>
    ;; void exit(int status);
    ;; void _Exit(int status);
    ;; #include <unistd.h>
    ;; _exit(int status);
    "exit" "_Exit" "_exit"
    ;; atexit Function
    ;; #include <stdlib.h>
    ;; int atexit(void (*func)(void));
    ;;     returns: 0 if OK, nonzero on error
    "atexit"
    ;; 7.5 Environment List
    ;; extern char **environ;
    "environ"
    ;; 7.8 Memory Allocation
    ;; #include <stdlib.h>
    ;; void *malloc(size_t size);
    ;; void *calloc(size_t nobj, size_t size);
    ;; void *realloc(void *ptr, size_t newsize);
    ;;     all three return: non-null pointer if OK, NULL on error
    ;; void free(void *ptr);
    "malloc" "calloc" "realloc" "free"
    ;; 7.9 Environment Variables
    ;; #include <stdlib.h>
    ;; char *getenv(const char *name);
    ;;     returns: pointer to value associated with name, NULL if not found
    "getenv"
    ;; Figure 7.7 Envirnment variables defined int eh Single UNIX Specification
    "COLUMNS" "DATEMSK" "HOME" "LANG" "LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MESSAGES"
    "LC_MONETARY" "LC_NUMERIC" "LC_TIME" "LINES" "LOGNAME" "MSGVERB" "LNSPATH" "PATH"
    "PWD" "SHELL" "TERM" "TMPDIR" "TZ"
    ;; #include <stdlib.h>
    ;; int putenv(char *str);
    ;; int setenv(const char *name, const char *value, int rewrite);
    ;; int unsetenv(const char *name);
    ;;     all return: 0 if OK, nonzero on error
    "putenv" "setenv" "unsetenv"
    "clearenv"  ;; supported only on Linux
    ;; 7.10 setjmp and longjmp Functions
    ;; #include <setjmp.h>
    ;; int setjmp(jmp_buf env);
    ;;     returns: 0 if called directly, nonzero if returning from a call to longjmp
    ;; void long(jmp_buf env, int val);
    "jmp_buf"
    "setjmp" "longjmp"
    ;; 7.11 getrlimit and setrlimit Functions
    ;; #include <sys/resource.h>
    ;; int getrlimit(int resource, struct rlimit *rlptr);
    ;; int setrlimit(int resource, const struct rlimit *rlptr);
    ;;     both return: 0 if OK, nonzero on error
    "getrlimit" "setrlimit"
    ;; struct rlimit {
    ;;     rlim_t rlim_cur;    /* soft limit: current limit */
    ;;     rlim_t rlim_max;    /* hard limit: maximum value for rlim_cur */
    ;; };
    "rlimit" "rlim_cur" "rlim_max"
    ;; resource argument of getrlimit() and setrlimit():
    "RLIMIT_AS" "RLIMIT_CORE" "RLIMIT_CPU" "RLIMIT_DATA" "RLIMIT_FSIZE"
    "RLIMIT_LOCKS" "RLIMIT_MEMLOCK" "RLIMIT_NOFILE"  "RLIMIT_NPROC"
    "RLIMIT_RSS" "RLIMIT_SBSIZE" "RLIMIT_STACK" "RLIMIT_VMEM"
    ;;
    ;; * Chapter 8 Process Control
    ;; 8.2 Process Identifiers
    ;; #include <unistd.h>
    ;; pid_t getpid(void);
    ;;     returns: process ID of calling process
    ;; pid_t getppid(void);
    ;;     returns: parent process ID of calling process
    ;; uid_t getuid(void);
    ;;     returns: real user ID of calling process
    ;; uid_t geteuid(void);
    ;;     returns: effective user ID of calling process
    ;; gid_t getgid(void);
    ;;     returns: real group ID of calling process
    ;; gid_t getegid(void);
    ;;     returns: effective group ID of calling process
    "getpid" "getppid" "getuid" "geteuid" "getgid" "getegid"
    ;; 8.3 fork Function
    ;; #include <unistd.h>
    ;; pid_t fork(void);
    ;;     returns: 0 in child, process ID of child in parent, -1 on error
    "fork"
    ;; pid_t vfork(void);
    ;;     returns: 0 in child, process ID of child in parent, -1 on error
    ;; marked as an obselete interface in SUSv3
    "vfork"
    ;; 8.6 wait and waitpid Functions
    ;; #include <sys/wait.h>
    ;; pid_t wait(int *statloc);
    ;; pid_t waitpid(pid_t pid, int *statloc, int options);
    ;;     both return: process ID if OK, 0(see later), or -1 on error
    "wait" "waitpid"
    ;; Figure 8.4 Macros to examine the termination status returned by wait and waitpid
    "WIFEXITED" "WEXITSTATUS"
    "WIFSIGNALED" "WTERMSIG" "WCOREDUMP"
    "WIFSTOPPED" "WSTOPSIG"
    "WIFCONTINUED"
    ;; Figure 8.7 The options constants for waitpid
    "WCONTINUED" "WNOHANG" "WUNTRACED"
    ;; 8.7 waitid Function
    ;; #include <sys/wait.h>
    ;; int waitid(idtype_t idtype, id_t id, siginfo_t *infop, int options);
    ;;     returns: 0 if OK, -1 on error
    "waitid"
    ;; Figure 8.9 The idtype constants for waitid
    "P_PID" "P_PGID" "P_ALL"
    ;; Figure 8.10 The options constants for waitid
    "WCONTINUED" "WEXITED" "WNOHANG" "WNOWAIT" "WSTOPPED"
    ;; 8.8 wait3 and wait4 Functions
    ;; #include <sys/types.h>
    ;; #include <sys/wait.h>
    ;; #include <sys/time.h>
    ;; #include <sys/resource.h>
    ;; pid_t wait3(int *statloc, int options, struct rusage *rusage);
    ;; pid_t wait4(pid_t pid, int *statloc, int options, struct rusage *rusage);
    ;;     both return: process ID if OK, 0, or -1 on error
    "wait3" "wait4"
    "rusage"
    ;; 8.10 exec Functions
    ;; #include <unistd.h>
    ;; int execl(const char *pathname, const char *arg0, ... /* (char *)0 */ );
    ;; int execv(const char *pathname, char *const argv[]);
    ;; int execle(const char *pathname, const char *arg0, ... /* (char *)0, char *const envp[] */ );
    ;; int execve(const char *pathname, char *const argv[], char *const envp[]);
    ;; int execlp(const char *filename, const char *arg0, ... /* (char *)0 */ );
    ;; int execvp(const char *filename, char *const argv[]);
    ;;     all six return: -1 on error, no return on success
    "execl" "execv" "execle" "execve" "execlp" "execvp"
    ;; 8.11 Changing User IDs and Group IDs
    ;; #include <unistd.h>
    ;; int setuid(uid_t uid);
    ;; int setgid(gid_t gid);
    ;;     both return: 0 if OK, -1 on error
    "setuid" "setgid"
    ;; setreuid and setregid Functions
    ;; #include <unistd.h>
    ;; int setreuid(uid_t ruid, uid_t euid);
    ;; int setregid(gid_t rgid, gid_t egid);
    ;;     both return: 0 if OK, -1 on error
    "setreuid" "setregid"
    ;; seteuid and setugid Functions
    ;; #include <unistd.h>
    ;; int seteuid(uid_t uid);
    ;; int setegid(gid_t gid);
    ;;     both return: 0 if OK, -1 on error
    "seteuid" "setegid"
    ;; 8.13 system Function
    ;; #include <stdlib.h>
    ;; int system(const char *cmdstring);
    ;;     returns: 01, 127, or termination status of cmdstring
    "system"
    ;; 8.14 Process Accounting
    ;; #include <sys/acct.h>
    ;; typedef u_short comp_t;  /* 3-bit base 8 exponent; 13-bit fraction */
    ;; struct acct {
    ;;     char ac_flag;     /* AFORK, ASU, ACOMPAT, ACORE, AXSIG, AEXPND */
    ;;     char ac_stat;     /* termination status (signal & core flag only) */
    ;;                       /* (Solaris only)*/
    ;;     uid_t ac_uid;     /* real user ID */
    ;;     gid_t ac_gid;     /* real group ID */
    ;;     dev_t ac_tty;     /* controlling terminal */
    ;;     time_t ac_btime;  /* starting calendar time */
    ;;     comp_t ac_utime;  /* user CPU time (clock ticks) */
    ;;     comp_t ac_stime;  /* system CPU time (clock ticks) */
    ;;     comp_t ac_etime;  /* elapsed time (clock ticks) */
    ;;     comp_t ac_mem;    /* average memory usage */
    ;;     comp_t ac_io;     /* bytes transferred (by read and write) */
    ;;                       /* "blocks" on BSD systems */
    ;;     comp_t ac_rw;     /* blocks read or written */
    ;;                       /* (not present on BSD systems) */
    ;;     char ac_comm[8];  /* command name: [8] for Solaris, */
    ;;                       /* [10] for Mac OS X, [16] for FreeBSD, and */
    ;;                       /* [17] for Linux */
    ;; }
    "comp_t"
    "acct"
    "ac_flag" "ac_stat" "ac_uid" "ac_gid" "ac_tty"
    "ac_btime" "ac_utime" "ac_stime" "ac_etime" "ac_mem" "ac_io" "ac_rw"
    "ac_comm"
    ;; Figure 8.26 Values for ac_flag from accounting record
    "AFORK" "ASU" "ACOMPAT" "ACORE" "AXSIG" "AEXPND"
    ;; 8.15 User Identification
    ;; #include <unistd.h>
    ;; char *getlogin(void);
    ;;     returns: pointer to string giving login name if OK, NULL on error
    "getlogin"
    ;; 8.16 Process Times
    ;; #include <sys/time.h>
    ;; clock_t times(struct tms *buf);
    ;;     returns: elapsed wall clock time in clock ticks if OK, -1 on error
    "times"
    ;; struct tms {
    ;;     clock_t tms_utime;    /* user CPU time */
    ;;     clock_t tms_stime;    /* system CPU time */
    ;;     clock_t tms_cutime;   /* user CPU time, terminated children */
    ;;     clock_t tms_cstime;   /* system CPU time, terminated children */
    ;; }
    "tms"
    "tms_utime" "tms_stime" "tms_cutime" "tms_cstime"
    ;;
    ;; * Chapter 9 Process Relationships
    ;; 9.4 Process Groups
    ;; #include <unistd.h>
    ;; pid_t getpgrp(void);
    ;;     returns: process group ID of calling process
    "getpgrp"
    ;; #include <unistd.h>
    ;; pid_t getpgid(pid_t pid);
    ;;     returns: process group ID if OK, -1 on error
    "getpgid"
    ;; #include <unistd.h>
    ;; int setpgid(pid_t pid, pid_t pgid);
    ;;     returns: 0 if OK, -1 on error
    "setpgid"
    ;; #include <unistd.h>
    ;; pid_t setsid(void);
    ;;     returns: process group ID if OK, -1 on error
    "setsid"
    ;; #include <unistd.h>
    ;; pid_t getsid(pid_t pid);
    ;;     returns: session leader's process group ID if OK, -1 on error
    "getsid"
    ;; 9.7 tcgetpgrp, tcsetpgrp, tcgetsid Functions
    ;; #include <unistd.h>
    ;; pid_t tcgetpgrp(int filedes);
    ;;     returns: process group ID of foreground process group if OK, -1 on error
    ;; int tcsetpgrp(int filedes, pid_t pgrpid);
    ;;     returns: 0 if OK, -1 on error
    "tcgetpgrp" "tcsetpgrp"
    ;; #include <termios.h>
    ;; pid_t tcgetsid(int filedes);
    ;;     returns: session leader's process group ID if OK, -1 on error
    "tcgetsid"
    ;;
    ;; * Chapter 10 Signals
    ;; 10.2 Signal Concepts
    ;; Figure 10.1 UNIX System signals
    "SIGABRT" "SIGALRM" "SIGBUS" "SIGCANCEL" "SIGCHLD" "SIGCONT" "SIGEMT"
    "SIGFPE" "SIGFREEZE" "SIGHUG" "SIGILL" "SIGINFO" "SIGINT" "SIGIO"
    "SIGIOT" "SIGKILL" "SIGLWP" "SIGPIPE" "SIGPOLL" "SIGPROF" "SIGPWR"
    "SIGQUIT" "SIGSEGV" "SIGSTKFLT" "SIGSTOP" "SIGSYS" "SIGTERM" "SIGTHAW"
    "SIGTRAP" "SIGTSTP" "SIGTTIN" "SIGTTOU" "SIGURG" "SIGUSR1" "SIGUSR2"
    "SIGVTALRM" "SIGWAITING" "SIGWINCH" "SIGXCPU" "SIGXFSZ" "SIGXRES"
    ;; 10.3 signal Function
    ;; #include <signal.h>
    ;; void (*signal(int signo, void (*func)(int))) (int);
    ;;     returns: previous disposition of signal (see following) if OK,
    ;;              SIG_ERR on error
    "signal"
    ;; typedef void Sigfunc(int);
    "Sigfunc"
    ;; #include <signal.h>
    ;; typedef void (*sighandler_t)(int);
    ;; sighandler_t sigset(int sig, sighandler_t disp);
    "sigset"
    ;; #include <signal.h>
    ;; int sigvec(int sig, struct sigvec *vec, struct sigvec *ovec);
    "sigvec"
    ;; 10.7 SIGCLD Semantics
    "SIGCLD"
    ;; 10.9 kill and raise Functions
    ;; #include <signal.h>
    ;; int kill(pid_t pid, int signo);
    ;; int raise(int signo);
    ;;     both return: 0 if OK, -1 on error
    "kill" "raise"
    ;; 10.10 alarm and pause Functions
    ;; #include <unistd.h>
    ;; unsigned int alarm(unsigned int seconds);
    ;;     returns: 0 or number of seconds until previously set alarm
    "alarm"
    ;; #include <unistd.h>
    ;; int pause(void);
    ;;     returns: -1 with errno set to EINTR
    "pause"
    ;; 10.11 Signal Sets
    ;; #include <signal.h>
    ;; int sigemptyset(sigset_t *set);
    ;; int sigfillset(sigset_t *set);
    ;; int sigaddset(sigset_t *set, int signo);
    ;; int sigdelset(sigset_t *set, int signo);
    ;;     all four return: 0 if OK, -1 on error
    "sigset_t"
    "sigemptyset" "sigfillset" "sigaddset" "sigdelset"
    ;; int sigismember(const sigset_t *set, int signo);
    ;;     returns: 1 if true, 0 if false, -1 on error
    "sigismember"
    ;; 10.12 sigprocmask Function
    ;; #include <signal.h>
    ;; int sigprocmask(int how, const sigset_t *restrict set,
    ;;                 sigset_t * restrict oset);
    ;;     returns: 0 if OK, -1 on error
    "sigprocmask"
    ;; Figure 10.13 Ways to change current signal mask using sigprocmask
    "SIG_BLOCK" "SIG_UNBLOCK" "SIG_SETMASK"
    ;; 10.13 sigpending Function
    ;; #include <signal.h>
    ;; int sigpending(sigset_t *set);
    ;;     returns: 0 if OK, -1 on error
    "sigpending"
    ;; 10.14 sigaction Function
    ;; #include <signal.h>
    ;; int sigaction(int signo, const struct sigaction *restrict act,
    ;;               struct sigaction *restrict oact);
    ;;     returns: 0 if OK, -1 on error
    "sigaction"
    ;; struct sigaction {
    ;;     void     (*sa_handler)(int);  /* addr of signal handler, */
    ;;                                   /* or SIG_IGN, or SIG_DFL */
    ;;     sigset_t sa_mask;             /* additional signals to block */
    ;;     int      sa_flags;            /* signal options, Figure 10.16 */
    ;;
    ;;     /* alternate handler */
    ;;     void     (*sa_sigaction)(int, siginfo_t *, void *);
    ;; }
    "sigaction"
    "sa_handler" "sa_mask" "sa_flags" "sa_sigaction"
    ;; struct siginfo {
    ;;     int    si_signo;   /* signal number */
    ;;     int    si_errno;   /* if nonzero, errno value from <errno.h> */
    ;;     int    si_code;    /* additional info (depends on signal) */
    ;;     pid_t  si_pid;     /* sending process ID */
    ;;     uid_t  si_uid;     /* sending process real user ID */
    ;;     void  *si_addr;    /* address that caused the fault */
    ;;     int    si_status;  /* exit value or signal number */
    ;;     long   si_band;    /* band number for SIGPOLL */
    ;;     /* posssibly other fields also */
    ;; }
    "siginfo"
    "si_signo" "si_errno" "si_code" "si_pid" "si_uid" "si_addr"
    "si_status" "si_band"
    ;; Figure 10.16 Option flags(sa_flags) for the handling of each signal
    "SA_INTERRUPT" "SA_NOCLDSTOP" "SA_NOCLDWAIT" "SA_NODEFER"
    "SA_ONSTACK" "SA_RESETHAND" "SA_RESTART" "SA_SIGINFO"
    ;; Figure 10.17 siginfo_t code values
    "SIGILL"
    "ILL_ILLOPC" "ILL_ILLOPN" "ILL_ILLADR" "ILL_ILLTRP" "ILL_PRVOPC"
    "ILL_PRVREG" "ILL_COPROC" "ILL_BADSTK"
    "SIGFPE"
    "FPE_INTDIV" "FPE_INTOVF" "FPE_FLTDIV" "FPE_FLTOVF" "FPE_FLTUND"
    "FPE_FLTRES" "FPE_FLTINV" "FPE_FLTSUB"
    "SIGSEGV"
    "SEGV_MAPERR" "SEGV_ACCERR"
    "SIGBUS"
    "BUS_ADRALN" "BUS_ADRERR" "BUS_OBJERR"
    "SIGTRAP"
    "TRAP_BRKPT" "TRAP_TRACE"
    "SIGCHLD"
    "CLD_EXITED" "CLD_KILLED" "CLD_DUMPED" "CLD_TRAPPED"
    "CLD_STOPPED" "CLD_CONTINUED"
    "SIGPOLL"
    "POLL_IN" "POLL_OUT" "POLL_MSG" "POLL_ERR" "POLL_ERR" "POLL_PRI" "POLL_HUP"
    "Any"
    "SI_USER" "SI_QUEUE" "SI_TIMER" "SI_ASYNCIO" "SI_MESEQ"
    ;; 10.15 sigsetjmp and siglongjmp Functions
    ;; #include <setjmp.h>
    ;; int sigsetjmp(sigjmp_buf env, int savemask);
    ;;     returns: 0 if called directly,
    ;;              nonzero if returning from a call to siglongjmp
    ;; void siglongjmp(sigjmp_buf env, int val);
    "sigjmp_buf"
    "sigsetjmp"
    "siglongjmp"
    ;; 10.16 sigsuspend Function
    ;; #include <signal.h>
    ;; int sigsuspend(const sigset_t *sigmask);
    "sigsuspend"
    ;; 10.17 abort Function
    ;; #include <stdlib.h>
    ;; void abort(void);
    ;;     This function never returns
    "abort"
    ;; 10.19 sleep Function
    ;; #include <unistd.h>
    ;; unsigned int sleep(unsigned int seconds);
    "sleep"
    ;; 10.21 Additional Functions
    ;; Signal Names:
    ;; FreeBSD 5.2.1, Linux 2.4.22, and Mac OS X 10.3
    ;; extern char *sys_siglist[];
    ;; Solaris
    ;; extern char *_sys_siglist[];
    "sys_siglist" "_sys_siglist"
    ;; #include <signal.h>
    ;; void psignal(int signo, const char *msg);
    "psignal"
    ;; #include <string.h>
    ;; char *strsignal(int signo);
    ;;     returns: a pointer to a string describing the signal
    "strsignal"
    ;; Signal Mappings
    ;; Solaris
    ;; #include <signal.h>
    ;; int sig2str(int signo, char *str);
    ;; int str2sig(const char *str, int *signop);
    ;;     both return: 0 if OK, -1 on error
    "sig2str" "str2sig"
    ;;
    ;; * Chapter 11 Threads
    ;; 11.2 Thread Concepts
    ;; featuer test macro for POSIX thread
    "_POSIX_THREADS" "_SC_THREADS"
    ;; 11.3 Thread Identification
    ;; #include <pthread.h>
    ;; int pthread_equal(pthread_t tid1, pthread_t tid2);
    ;;     returns: nonzero if equal, 0 otherwise
    "pthread_t"
    "pthread_equal"
    ;; #include <pthread.h>
    ;; pthread_t pthread_self(void);
    ;;     returns: thre thread ID of the calling thread
    "pthread_self"
    ;; #include <pthread.h>
    ;; int pthread_create(pthread_t *restrict tidp,
    ;;                    const pthread_attr_t *restrict attr,
    ;;                    void *(*start_rtn)(void *), void *restrict arg);
    ;;     returns: 0 if OK, error number on failure
    "pthread_attr_t"
    "pthread_create"
    ;; #include <pthread.h>
    ;; void pthread_exit(void *rval_ptr);
    "pthread_exit"
    ;; #include <pthread.h>
    ;; int pthread_join(pthread_t thread, void **rval_ptr);
    ;;     returns: 0 if OK, error number on failure
    "pthread_join"
    "PTHREAD_CANCELED"
    ;; #include <pthread.h>
    ;; int pthread_cancel(pthread_t tid);
    ;;     returns: 0 if OK, error number on failure
    "pthread_cancel"
    ;; #include <pthread.h>
    ;; void pthread_cleanup_push(void (*rtn)(void *), void *arg);
    ;; void pthread_cleanup_pop(int execute);
    "pthread_cleanup_push" "pthread_cleanup_pop"
    ;;
    ;; * Chapter 13 Daemon Processes
    ;; 13.4 Error Logging
    ;; #include <syslog.h>
    ;; void openlog(const char *ident, int option, int facility);
    ;; void syslog(int priority, const char *format, ...);
    ;; void closelog(void);
    ;; int setlogmask(int maskpri);
    ;;     returns: previous log priority mask value
    "openlog" "syslog" "closelog" "setlogmask"
    ;; Figure 13.3 The option argument for openlog
    "LOG_CONS" "LOG_NDELAY" "LOG_NOWAIT" "LOG_ODELAY" "LOG_PERROR" "LOG_PID"
    ;; Figure 13.4 The facility arugment for openlog
    "LOG_AUTH" "LOG_AUTHPRIV" "LOG_CRON" "LOG_DAEMON" "LOG_FTP" "LOG_KERN"
    "LOG_LOCAL0" "LOG_LOCAL1" "LOG_LOCAL2" "LOG_LOCAL3" "LOG_LOCAL4"
    "LOG_LOCAL5" "LOG_LOCAL6" "LOG_LOCAL7" "LOG_LPR" "LOG_MAIL" "LOG_NEW"
    "LOG_SYSLOG" "LOG_USER" "LOG_UUCP"
    ;; Figure 13.5 The syslog levels(ordered)
    "LOG_EMERG" "LOG_ALERT" "LOG_CRIT" "LOG_ERR" "LOG_WARNING"
    "LOG_NOTICE" "LOG_INFO" "LOG_DEBUG"
    ;; #include <syslog.h>
    ;; #include <stdarg.h>
    ;; void vsyslog(int priority, const char *format, va_list arg);
    "vsyslog"
    ;;
    ;; * Chapter 14 Advanced I/O
    ;; 14.3 Record Locking
    ;; #include <fcntl.h>
    ;; int fcntl(int filedes, int cmd, ... /* struct flock *flockptr  */ );
    ;;     returns: depends on cmd if OK(see following), -1 on error
    ;; struct flock {
    ;;     short l_type;    /* F_RDLCK, F_WRLCK, or F_UNLCK */
    ;;     off_t l_start;   /* offset in bytes, relative to l_whence */
    ;;     short l_whence;  /* SEEK_SET, SEEK_CUR, or SEEK_END */
    ;;     off_t l_len;     /* length, in bytes; 0 means lock to EOF */
    ;;     pid_t l_pid;     /* returned with F_GETFL */
    ;; };
    "flock"
    "l_type" "l_start" "l_whence" "l_len" "l_pid"
    ;; l_type value:
    "F_RDLCK" "F_WRLCK" "F_UNLCK"
    ;; cmd arguement of fcntl():
    "F_GETLK" "F_SETLK" "F_SETLKW"
    ;;
    ;; * Chapter 15 Interprocess Communication
    ;; 15.2 Pipes
    ;; #include <unistd.h>
    ;; int pipe(int filedes[2]);
    ;;     returns: 0 if OK, -1 on error
    "pipe"
    ;; 15.3 popen and pclose Functions
    ;; #include <stdio.h>
    ;; FILE *popen(const char *cmdstring, const char *type);
    ;;     returns: file pointer if OK, NULL on error
    ;; int pclose(FILE *fp);
    ;;     returns: termination status of cmdstring, or -1 on error
    "popen" "pclose"
    ;; 15.5 FIFOs
    ;; #include <sys/stat.h>
    ;; int mkfifo(const char *pathname, mode_t mode);
    ;;     returns: 0 if OK, -1 on error
    "mkfifo"
    ;; 15.6.1 Identifiers and Keys
    ;; #include <sys/ipc.h>
    ;; key_t ftok(const char *path, int id);
    ;;     returns: key if OK, (key_t)-1 on error
    "key_t"
    "ftok"
    ;; key and flag for three get functions (msgget, semget and shmget):
    "IPC_PRIVATE" "IPC_CREAT" "IPC_EXCL"
    ;; 15.6.2 Permission Structure
    ;; struct ipc_perm {
    ;;     uid_t  uid;   /* owner's effective user id */
    ;;     gid_t  gid;   /* owner's effective group id */
    ;;     uid_t  cuid;  /* creator's effective user id */
    ;;     gid_t  cgid;  /* creator's effective group id */
    ;;     mode_t mode;  /* access mode */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "ipc_perm"
    "uid" "gid" "cuid" "cgid" "mode"
    ;; 15.7 Message Queues
    ;; struct msqid_ds {
    ;;     struct ipc_perm    msg_perm;      /* see Section 15.6.2 */
    ;;     msgqnum_t          msg_qnum;      /* # of messages on queue */
    ;;     msglen_t           msg_qbytes;    /* max # of bytes on queue */
    ;;     pid_t              msg_lspid;     /* pid of last msgsnd() */
    ;;     pid_t              msg_lrpid;     /* pid of last msgrcv() */
    ;;     time_t             msg_stime;     /* last-msgsnd() time */
    ;;     time_t             msg_rtime;     /* last-msgrcv() time */
    ;;     time_t             msg_ctime;     /* last-change time */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "msqid_ds"
    "msg_perm" "msg_qnum" "msg_qbytes" "msg_lspid" "msg_lrpid"
    "msg_stime" "msg_rtime" "msg_ctime"
    ;; #include <sys/msg.h>
    ;; int msgget(key_t key, int flag);
    ;;     returns: message queue ID if OK, -1 on error
    "msgget"
    ;; #include <sys/msg.h>
    ;; int msgctl(int msqid, int cmd, struct msqid_ds *buf);
    ;;     returns: 0 if OK, -1 on error
    "msgctl"
    ;; cmd argument of msgctl:
    "IPC_STAT" "IPC_SET" "IPC_RMID"
    ;; #include <sys/msg.h>
    ;; int msgsnd(int msqid, const void *ptr, size_t nbytes, int flag);
    ;;     returns: 0 if OK, -1 on error
    "msgsnd"
    ;; ptr argument of msgsnd:
    ;; struct mymesg {
    ;;     long mtype;        /* positive message type */
    ;;     char mtext[512];   /* message data, of length nbytes */
    ;; }
    "mymesg"
    "mtype" "mtext"
    ;; ipc flag
    "IPC_NOWAIT"
    ;; #include <sys/msg.h>
    ;; ssize_t msgrcv(int msqid, void *ptr, size_t nbytes, long type, int flag);
    ;;     returns: size of data portion of message if OK, -1 on error
    "msgrcv"
    ;; 15.8 Semaphores
    ;; struct semid_ds {
    ;;     struct ipc_perm    sem_perm;     /* see Section 15.6.2 */
    ;;     unsigned short     sem_nsems;    /* # of semaphores in set */
    ;;     time_t             sem_otime;    /* last-semop() time */
    ;;     time_t             sem_ctime;    /* last-change time */
    ;;     .
    ;;     .
    ;;     .
    ;; }
    "semid_ds"
    "sem_perm" "sem_nsems" "sem_otime" "sem_ctime"
    ;; Anonymous structure represent semaphore:
    ;; struct {
    ;;     unsigned short semval;     /* semaphore value, always >= 0 */
    ;;     pid_t          sempid;     /* pid for last operation */
    ;;     unsigned short semncnt;    /* # processes awaiting semval>curval */
    ;;     unsigned short semzcnt;    /* # processes awaiting semval==0 */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "semval" "sempid" "semncnt" "semzcnt"
    ;; #include <sys/sem.h>
    ;; int semget(key_t key, int nsems, int flag);
    ;;     returns: semaphore ID if OK, -1 on error
    "semget"
    ;; #include <sys/sem.h>
    ;; int semctl(int semid, int semnu, int cmd, ... /* unioin semun arg*/);
    ;;     returns: (see following)
    "semctl"
    ;; union semun {
    ;;     int              val;    /* for SETVAL */
    ;;     struct semid_ds *buf;    /* for IPC_STAT and IPC_SET */
    ;;     unsigned short  *array;  /* for GETALL and SETALL */
    ;; };
    "semun"
    "val" "buf" "array"
    ;; cmd argument of semctl:
    "IPC_STAT" "IPC_SET" "IPC_RMID" "GETVAL" "SETVAL" "GETPID"
    "GETNCNT" "GETZCNT" "GETALL" "SETALL"
    ;; #include <sys/sem.h>
    ;; int semop(int semid, struct sembuf semoparray[], size_t nops);
    ;;     returns: 0 if OK, -1 on error
    "semop"
    ;; struct sembuf {
    ;;     unsigned short sem_num;    /* member # in set (0, 1, ..., nsems-1) */
    ;;     short          sem_op;     /* operation (negative, 0, or positive)*/
    ;;     short          sem_flg;    /* IPC_NOWAIT, SEM_UNDO */
    ;; };
    "sembuf"
    "sem_num" "sem_op" "sem_flg"
    ;; value of sembuf.sem_flg:
    "IPC_NOWAIT" "SEM_UNDO"
    ;; 15.9 Shared Memory
    ;; struct shmid_ds {
    ;;     struct ipc_perm  shm_perm;    /* see Section 15.6.2 */
    ;;     size_t           shm_segsz;   /* size of segment in bytes */
    ;;     pid_t            shm_lpid;    /* pid of last shmop() */
    ;;     pid_t            shm_cpid;    /* pid of creator */
    ;;     shmatt_t         shm_nattch;  /* number of current attaches */
    ;;     time_t           shm_atime;   /* last-attch time */
    ;;     time_t           shm_dtime;   /* last-detach time */
    ;;     time_t           shm_ctime;   /* last-change time */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "shmid_ds"
    "shmatt_t"
    "shm_perm" "shm_segsz" "shm_lpid" "shm_cpid" "shm_nattch"
    "shm_atime" "shm_dtime" "shm_ctime"
    ;; #include <sys/shm.h>
    ;; int shmget(key_t key, size_t size, int flag);
    ;;     returns: shared memory ID if OK, -1 on error
    "shmget"
    ;; #include <sys/shm.h>
    ;; int shmctl(int shmid, int cmd, struct shmid_ds *buf);
    ;;     returns: 0 if OK, -1 on error
    "shmctl"
    ;; cmd arguemtn of shmctl:
    "IPC_STAT" "IPC_SET" "IPC_RMID"
    ;;
    ;;two additional command are provided by Linux and Solaris
    "SHM_LOCK" "SHM_UNLOCK"
    ;; #include <sys/shm.h>
    ;; void *shmat(int shmid, const void *addr, int flag);
    ;;     returns: pointer to shared memory segment if OK, -1 on error
    "shmat"
    ;; flag argument of shmat
    "SHM_RND" "SHM_RDONLY"
    ;; #include <sys/shm.h>
    ;; int shmdt(void *addr);
    ;;     returns: 0 if OK, -1 on error
    "shmdt"
    ;;
    ;; * Chapter 16 Network IPC: Sockets
    ;; 16.2 Socket Descriptors
    ;; #include <sys/socket.h>
    ;; int socket(int domain, int type, int protocol);
    ;;     returns: file(socket) descriptor if OK, -1 on error
    "socket"
    ;; domain argument of socket:
    "AF_INET" "AF_INET6" "AF_UNIX" "AF_LOCAL" "AF_UNSPEC"
    ;; type argument of socket:
    "SOCK_DGRAM" "SOCK_RAW" "SOCK_SEQPACKET" "SOCK_STREAM"
    ;; #include <sys/socket.h>
    ;; int shutdown(int sockfd, int how);
    ;;     returns: 0 if OK, -1 on error
    "shutdown"
    ;; 16.3 Addressing
    ;; 16.3.1 Byte Ordering
    ;; #include <arpa/inet.h>
    ;; uint32_t htonl(uint32_t hostint32);
    ;;     returns: 32-bit integer in network byte order
    ;; uint16_t htons(uint16_t hostint16);
    ;;     returns: 16-bit integer in network byte order
    ;; uint32_t ntohl(uint32_t netint32);
    ;;     returns: 32-bit integer in host byte order
    ;; uint16_t ntohs(uint16_t netint16);
    ;;     returns: 16-bit integer in host byte order
    "htonl" "htons" "ntohl" "ntohs"
    ;; 16.3.2 Address Formats
    ;; #include <netinet/in.h>
    ;; struct sockaddr {
    ;;     sa_family_t sa_family;    /* address family */
    ;;     char        sa_data[];    /* variable-length address */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "sockaddr"
    "sa_family" "sa_data"
    ;; struct in_addr {
    ;;     in_addr_t    s_addr;     /* IPv4 address */
    ;; };
    "in_addr"
    "in_addr_t"
    "s_addr"
    ;; struct sockaddr_in {
    ;;     sa_family_t    sin_family;    /* address family */
    ;;     in_port_t      sin_port;      /* port number */
    ;;     struct in_addr sin_addr;      /* IPv4 address */
    ;; };
    "sockaddr_in"
    "sa_family_t" "in_port_t"
    "sin_family" "sin_port" "sin_addr"
    ;; struct in6_addr {
    ;;     uint8_t    s6_addr[16];    /* IPv6 address */
    ;; };
    "in6_addr"
    "s6_addr"
    ;; struct sockaddr_in6 {
    ;;     sa_family_t     sin6_family;    /* address family */
    ;;     in_port_t       sin6_port;      /* port number */
    ;;     uint32_t        sin6_flowinfo;  /* traffic class and flow info */
    ;;     struct in6_addr sin6_addr;      /* IPv6 address */
    ;;     uint32_t        sin6_scope_id;  /* set of interface for scope */
    ;; };
    "sockaddr_in6"
    "sin6_family" "sin6_port" "sin6_flowinfo" "sin6_addr" "sin6_scope_id"
    ;; #include <arpa/inet.h>
    ;; const char *inet_ntop(int domain, const void *restrict addr,
    ;;                       char *restrict str, socklen_t size);
    ;;     returns: pointer to address string on success, NULL on error
    ;; int inet_pton(int domain, const char *restrict str,
    ;;               void *restric adrr);
    ;;     returns: 1 on success, 0 if the format is invalid, or -1 on error
    "inet_ntop"
    "inet_pton"
    ;; size argument of inet_ntop:
    "INET_ADDRSTRLEN" "INET6_ADDRSTRLEN"
    ;; 16.3.3 Address Lookup
    ;; #include <netdb.h>
    ;; struct hostent *gethostent(void);
    ;;     returns: pointer if OK, NULL on error
    ;; void sethostent(int stayopen);
    ;; void endhostent(void);
    "gethostent" "sethostent" "endhostent"
    ;; struct hostent {
    ;;     char     *h_name;       /* name of host */
    ;;     char    **h_aliases;    /* pointer to alternate host name array */
    ;;     int       h_addrtype;   /* address type */
    ;;     int       h_length;     /* length in bytes of address */
    ;;     char     *h_addr_list;  /* pointer to array of network addresses */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "hostent"
    "h_name" "h_aliases" "h_addrtype" "h_length" "h_addr_list"
    ;; #include <netdb.h>
    ;; struct netent *getnetbyaddr(uint32_t net, int type);
    ;; struct netent *getnetbyname(const char *name);
    ;; struct netent *getnetent(void);
    ;;     all return: pointer if OK, NULL on error
    ;; void setnetent(int stayopen);
    ;; void endnetent(void);
    "getnetbyaddr" "getnetbyname"
    "getnetent" "setnetent" "endnetent"
    ;; struct netent {
    ;;     char     *n_name;      /* network name */
    ;;     char    **n_aliases;   /* alternate network name array pointer */
    ;;     int       n_addrtype;  /* address type */
    ;;     uint32_t  n_net;       /* network number */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "netent"
    "n_name" "n_aliases" "n_addrtype" "n_net"
    ;; #include <netdb.h>
    ;; struct protoent *getprotobyname(const char *name);
    ;; struct protoent *getprotobynumber(int proto);
    ;; struct protoent *getprotoent(void);
    ;;     all return: pointer if OK, NULL on error
    ;; void setprotoent(int stayopen);
    ;; void endprotoent(void);
    "getprotobyname" "getprotobynumber"
    "getprotoent" "setprotoent" "endprotoent"
    ;; struct protoent {
    ;;     char     *p_name;     /* protocol name */
    ;;     char    **p_aliases;  /* pointer to alternate protocol name array */
    ;;     int       p_proto;    /* protocol number */
    ;;     .
    ;;     .
    ;;     .
    ;; };
    "protoent"
    "p_name" "p_aliases" "p_proto"
    ;; #include <netdb.h>
    ;; struct servent *getservbyname(const char *name, const char *proto);
    ;; struct servent *getservbyport(int port, const char *proto);
    ;; struct servent *getservent(void);
    ;;     all return: pointer if OK, NULL on error
    ;; void setservent(int stayopen);
    ;; void endservent(void);
    "getservbyname" "getservbyport"
    "getservent" "setservent" "endservnet"
    ;; struct servent {
    ;;
    ;; }
    ;;
    ;; * Appendix B
    ;; B.2 Standard Error Routines
    ;; #include "apue.h"
    ;; void err_ret(const char *fmt, ...);
    ;; void err_sys(const char *fmt, ...);
    ;; void err_exit(int error, const char *fmt, ...);
    ;; void err_dump(const char *fmt, ...);
    ;; void err_msg(const char *fmt, ...);
    ;; void err_quit(const char *fmt, ...);
    "err_ret" "err_sys" "err_exit" "err_dump" "err_msg" "err_quit"
    ;; void log_open(const char *ident, int option ,int facility);
    ;; void log_ret(const char *fmt, ...);
    ;; void log_sys(const char *fmt, ...);
    ;; void log_msg(const char *fmt, ...);
    ;; void log_quit(const char *fmt, ...);
    "log_open" "log_ret" "log_sys" "log_msg" "log_quit"
    ))


(ac-define-source apue
  '((candidates . ac-apue-candidates)
    (symbol . "U")
    (cache)))


;;; define source for unp keywords

(defun ac-unp-candidates ()
  "A keyword candidates produce function for unp keywords."
  '(
    ;;
    ;;
    ;;; functions
    ;; byte manipulation
    "htons" "htonl" "ntohs" "ntohl" "bzero" "bcopy" "bcmp" "memset" "memcpy"
    "memcmp" "inet_aton" "inet_addr" "inet_ntoa" "inet_pton" "inet_ntop"
    ;; wrapper functions
    "sock_ntop" "Sock_ntop" "sock_bind_wild" "Sock_bind_wild"
    "sock_cmp_addr" "Sock_cmp_addr" "sock_cmp_port" "Sock_cmp_port"
    "sock_get_port" "Sock_get_port" "sock_ntop_host" "Sock_ntop_host"
    "sock_set_addr" "Sock_set_addr" "sock_set_port" "Sock_set_port"
    "sock_set_wild" "Sock_set_wild"
    ;; io basic
    "Read" "read" "Readline" "readline" "Write" "write"
    "Readn" "readn" "Writen" "writen"
    "Fgets" "fgets" "Fputs" "fputs"
    ;; elementary socket and tcp functions
    "Socket" "socket" "Connect" "connect" "Bind" "bind" "Listen" "listen"
    "Accept" "accpet" "Close" "close"
    ;; socket address
    "Getsockname" "getsockname" "Getpeername" "getpeername"
    ;; signal wrapper
    "Signal" "signal"
    ;; i/o multiplexing
    "Select" "select" "Shutdown" "shutdown" "Pselect" "pselect" "Poll" "poll"
    ;; socket options
    "Getsockopt" "getsockopt" "Setsockopt" "setsockopt"
    "Fcntl" "fcntl" "Ioctl" "ioctl"
    ;; elementary udp functions
    "Recvfrom" "recvfrom" "Sendto" "sendto"
    ;; name and address conversions
    "Gethostbyname" "gethostbyname" "Gethostbyaddr" "gethostbyaddr"
    "Getservbyname" "getservbyname" "Getservbyport" "getservbyport"
    "Getaddrinfo" "getaddrinfo" "Freeaddrinfo" "freeaddrinfo"
    "Getnameinfo" "getnameinfo"
    ;; re-entrant functions
    "Gethostbyname_r" "gethostbyname_r" "Gethostbyaddr_r" "gethostbyaddr_r"
    ;; name and address conversion wrapper
    "Host_serv" "host_serv"
    "Tcp_connect" "tcp_connect" "Tcp_listen" "tcp_listen"
    "Udp_client" "udp_client" "Udp_connect" "udp_connect"
    "Udp_server" "udp_server"
    ;; Obsolete ipv6 address lookup functions
    "Gethostbyname2" "gethostbyname2" "Getipnodebyname" "getipnodebyname"
    "Freehostent" "freehostent"
    ;; daemon processes and the inetd superserver
    "Syslog" "Openlog" "Closelog" "Daemon_init" "Daemon_inetd"
    "syslog" "openlog" "closelog" "daemon_init" "daemon_inetd"
    ;; threads
    ;; basic thread functions
    "Pthread_create" "Pthread_join" "Pthread_self" "Pthread_exit"
    "pthread_create" "pthread_join" "pthread_self" "pthread_exit"
    ;; thread-specific data
    "Pthread_once" "pthread_once" "Pthread_key_create" "pthread_key_create"
    "Pthread_getspecific" "Pthread_setspecific"
    "pthread_getspecific" "pthread_setspecific"
    ;; thread mutex and condition variable
    "Pthread_mutex_lock" "Pthread_mutex_unlock" "Pthread_mutex_init"
    "pthread_mutex_lock" "pthread_mutex_unlock" "pthread_mutex_init"
    "Pthread_cond_wait" "pthread_cond_wait"
    "Pthread_cond_signal" "pthread_cond_signal"
    "Pthread_cond_broadcast" "pthread_cond_broadcast"
    "Pthread_cond_timedwait" "pthread_cond_timedwait"
    ;;
    ;;
    ;;; macros and variables
    ;; socket address structures
    "INET_ADDRSTRLEN" "INET6_ADDRSTRLEN" "SIN6_LEN"
    "AF_INET" "AF_INET6" "AF_LOCAL" "AF_ROUTE" "AF_KEY"
    "SOCK_STREAM" "SOCK_DGRAM" "SOCK_SEQ" "SOCK_SEQPACKET" "SOCK_PACKET" "SOCK_RAW"
    "IPPROTO_TCP" "IPPROTO_UDP" "IPPROTO_SCTP"
    "INADDR_ANY" "IN6ADDR_ANY_INIT"
    "in6addr_any"
    ;; select() fd_set operator
    "FD_ZERO" "FD_SET" "FD_CLR" "FD_ISSET" "FD_SETSIZE"
    ;; shutdown() argument howto
    "SHUT_RD" "SHUT_WR" "SHUT_RDWR"
    ;; poll() pollfd.events
    "POLLIN" "POLLRDNORM" "POLLRDBAND" "POLLPRI" "POLLOUT" "POLLWRNORM"
    "POLLWRBAND" "POLLERR" "POLLHUP" "POLLNVAL"
    ;; poll() timeout (doesn't exist on linux)
    "INFTIM"
    ;; socket options
    ;; -- option level
    "SOL_SOCKET" "IPPROTO_IP" "IPPROTO_ICMPV6" "IPPROTO_IPV6" "IPPROTO_TCP"
    "IPPROTO_SCTP"
    ;; -- options under SOL_SOCKET level
    "SO_BROADCAST" "SO_DEBUG" "SO_DONTROUTE" "SO_ERROR" "SO_KEEPALIVE"
    "SO_LINGER" "SO_OOBINLINE" "SO_RCVBUF" "SO_SNDBUF" "SO_RCVLOWAT"
    "SO_SNDLOWAT" "SO_RCVTIMEO" "SO_SNDTIMEO" "SO_REUSEADDR" "SO_REUSEPORT"
    "SO_TYPES" "SO_USELOOPBACK"
    ;; -- options under IPPROTO_IP level
    "IP_HDRINCL" "IP_OPTIONS" "IP_RECVDSTADDR" "IP_RECVIF" "IP_TOS" "IP_TTL"
    "IP_MULTICAST_IF" "IP_MULTICAST_TTL" "IP_MULTICAST_LOOP"
    "IP_ADD_MEMBERSHIP" "IP_DROP_MEMBERSHIP" "IP_BLOCK_SOURCE"
    "IP_UNBLOCK_SOURCE" "IP_ADD_SOURCE_MEMBERSHIP" "IP_DROP_SOURCE_MEMBERSHIP"
    ;; -- options under IPPROTO_ICMPV6 level
    "ICMP6_FILTER"
    ;; -- options under IPPROTO_IPV6 level
    "IPV6_CHECKSUM" "IPV6_DONTFRAG" "IPV6_NEXTHOP"
    "IPV6_PATHMTU" "IPV6_RECVDSTOPTS" "IPV6_RECVHOPLIMIT" "IPV6_RECVHOPOPTS"
    "IPV6_RECVPATHMTU" "IPV6_RECVPKTINFO" "IPV6_RECVRTHDR" "IPV6_RECVTCLASS"
    "IPV6_UNICAST_HOPS" "IPV6_USE_MIN_MTU" "IPV6_V6ONLY"
    "IPV6_MULTICAST_IF" "IPV6_MULTICAST_HOPS" "IPV6_MULTICAST_LOOP"
    "IPV6_JOIN_GROUP" "IPV6_LEAVE_GROUP"
    ;; -- options under IPPROTO_IP or IPPROTO_IPV6 level
    "MCAST_JOIN_GROUP" "MCAST_LEAVE_GROUP" "MCAST_BLOCK_SOURCE"
    "MCAST_UNBLOCK_SOURCE" "MCAST_JOIN_SOURCE_GROUP" "MCAST_LEAVE_SOURCE_GROUP"
    ;; -- options under IPPROTO_TCP level
    "TCP_MAXSEG" "TCP_NODELAY"
    ;; -- options under IPPROTO_SCTP level
    "SCTP_ADAPTION_LAYER" "SCTP_ASSOCINFO" "SCTP_AUTOCLOSE"
    "SCTP_DEFAULT_SEND_PARAM" "SCTP_DISABLE_FRAGMENTS" "SCTP_EVENTS"
    "SCTP_GET_PEER_ADDR_INFO" "SCTP_I_WANT_MAPPED_V4_ADDR" "SCTP_INIMSG"
    "SCTP_MAXBURST" "SCTP_MAXSEG" "SCTP_NODEALY" "SCTP_PEER_ADDR_PARAMS"
    "SCTP_PRIMARY_ADDR" "SCTP_RTOINFO" "SCTP_SET_PEER_PRIMARY_ADDR"
    "SCTP_STATUS"
    ;; fcntl() cmds and flags related with socket
    "F_GETFL" "F_SETFL" "F_GEOWN" "F_SETOWN" "O_NONBLOCK" "O_ASYNC"
    ;; ipv4 and ipv6 interoperability
    ;; ipv6 address-testing macros
    ;; test the basic type of ipv6 address
    "IN6_IS_ADDR_UNSPECIFIED" "IN6_IS_ADDR_LOOPBACK" "IN6_IS_ADDR_MULTICAST"
    "IN6_IS_ADDR_LINKLOCAL" "IN6_IS_ADDR_SITELOCAL" "IN6_IS_ADDR_V4MAPPED"
    "IN6_IS_ADDR_V4COMPAT"
    ;; test the scopt of an ipv6 multicast address
    "IN6_IS_ADDR_MC_NODELOCAL" "IN6_IS_ADDR_MC_LINKLOCAL"
    "IN6_IS_ADDR_MC_SITELOCAL" "IN6_IS_ADDR_MC_ORGLOCAL"
    "IN6_IS_ADDR_MC_GLOBAL"
    ;; daemon processes and the inetd superserver
    ;; syslog() message level
    "LOG_EMERG" "LOG_ALERT" "LOG_CRIT" "LOG_ERR" "LOG_WARNING" "LOG_NOTICE"
    "LOG_INFO" "LOG_DEBUG"
    ;; syslog() message facility
    "LOG_AUTH" "LOG_AUTHPRIV" "LOG_CRON" "LOG_DAEMON" "LOG_FTP" "LOG_KERN"
    "LOG_LOCAL0" "LOG_LOCAL1" "LOG_LOCAL2" "LOG_LOCAL3" "LOG_LOCAL4"
    "LOG_LOCAL5" "LOG_LOCAL6" "LOG_LOCAL7" "LOG_LPR" "LOG_MAIL" "LOG_NEWS"
    "LOG_SYSLOG" "LOG_USER" "LOG_UUCP"
    ;; openlog() option
    "LOG_CONS" "LOG_NDELAY" "LOG_PERROR" "LOG_PID"
    ;; thread  related
    "PTHREAD_ONCE_INIT" "PTHREAD_MUTEX_INITIALIZER"
    ;;
    ;;
    ;;; types, structs
    ;; socket address structures
    "sockaddr_in" "socklen_t" "sa_family_t"  "in_addr"
    "sockaddr"
    "sockaddr6_in"  "in6_addr"
    "sockaddr_storage"
    ;; signal handler prototype
    "Sigfunc"
    ;; select() argument types
    "fd_set" "timeval"
    ;; pselect() argument types
    "timespec" "sigset_t"
    ;; poll() argument types
    "pollfd"
    ;; SO_LINGER option
    "linger"
    ;; thread
    "pthread_t" "pthread_attr_t"
    "pthread_once_t" "pthread_key_t"
    "pthread_mutex_t" "pthread_cond_t"
    ;;
    ;;
    ;;; struct members
    ;; socket address structures
    "sin_len" "sin_family" "sin_port" "sin_addr" "sin_zero" "s_addr"
    "sa_len" "sa_family" "sa_data"
    "sin6_len" "sin6_family" "sin6_port" "sin6_flowinfo" "sin6_addr"
    "sin6_scope_id" "s6_addr"
    "ss_len" "ss_family"
    ;; select() struct timeval members
    "tv_sec" "tv_usec"
    ;; pselect() struct timespec members
    "tv_nsec"
    ;; poll() struct pollfd members
    "fd" "events" "revents"
    ;; SO_LINGER option linger members
    "l_onoff" "l_linger"
    ))


(ac-define-source unp
  '((candidates . ac-unp-candidates)
    (symbol . "N")
    (cache)))


(provide 'auto-complete-cc)

;;; auto-complete-cc.el ends here
