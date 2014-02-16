;; -*- Emacs-Lisp -*-
;; Settings for `font-lock-mode'.

;; Copyright (C) 2011 Dylan.Wen

;; Author: Dylan.Wen <hhkbp2@gmail.com>
;; Time-stamp: <2014-02-17 00:58>

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


;; load dev bases
(require 'dev-base-settings)

;; load customized faces of font-lock
(require 'font-lock-face)

;; load pretty font-lock features
(require 'font-lock-pretty)


(defcustom font-lock-keywords-dev
  '(;; tags
    ("\\<\\(TODO\\|Todo\\|ToDo\\|FIXME\\|HACK\\)\\>"
     1 font-lock-warning-face prepend)
    ;; constants in c/c++ style
    ;; integer type (hexadecimal)
    ("\\W\\([-+]?0x[0-9A-Fa-f]+[Ll]?[Uu]?\\)\\_>"
     1 font-lock-constant-face)
    ;; floating point type (decimal point/exponent)
    ;; This precedes decimal integer type because first part of
    ;; floating point type matches the regex of decimal integer.
    ;; As a consequence of keywords highlighting process,
    ;; (which is just a big loop searching the keywords along down the text)
    ;; we need to place the regexs properly, just like the way
    ;; positioning the alternative structure while writing regular expressions.
    ("\\W\\([-+]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[Ee][0-9]+\\)?[FfDd]?\\)"
     1 font-lock-constant-face)
    ;; integer type (decimal/octal)
    ("\\W\\([-+]?[0-9]+[Ll]?[Uu]?\\)"
     1 font-lock-constant-face)
    ;; character (literal/escape/octal/hexadecimal format)
    ("\\('\\(?:[a-zA-Z0-9]\\|\\\\\\(?:[abfnrtv\\?'\"]\\|[0-7]\\{1,3\\}\\|x[0-9a-fA-F]\\{1,2\\}\\)\\)'\\)"
     1 font-lock-constant-face prepend)
    )

  "*A list of keywords to highlight in all software develop modes.
A user-level font-lock keywords to be added to the end of `font-lock-keywords'.
Refer to `font-lock-settings' for its usage.")


(defcustom font-lock-keywords-cc
  '(;; highlight parentheses in c/c++ modes
    ;; parentheses by default
    ("[](){}[]" . 'wlc/paren-default-face)
    ;; parentheses in pair
    (dw-match-cc-paren
     (0 'dw-pair-face prepend))
    )

  "*A list of keywords to highlight in c/c++ modes.")


(defcustom font-lock-keywords-libc
  '()

  "*A list of keywords in c standard library to highlight.")


(defcustom font-lock-keywords-libcpp
  '(
    ;; namespace
    ("\\_<\\(std\\)\\_>"
     1 'dw-keywords-cpp-namespace-face prepend)
    ;; standard library sequencial/associative container and container adaptor
    ("[^.]\\_<\\(\
string\\|vector\\|list\\|deque\\|stack\\|\\(?:priority_\\)?queue\\|\
\\(?:multi\\)?\\(?:map\\|set\\)\\|bitset\
\\)\\_>"
     1 'dw-keywords-cpp-std-container-face prepend)
    ;; container (and adaptor) related types definitions
    ("::\\_<\\(\
\\(?:size\\|value\\|difference\\)_type\\|\
\\(?:const_\\)?reference\\|\
npos\\|\
container_type\\|\
\\(?:key\\|mapped\\|value\\)_type\
\\)\\_>"
     1 'dw-keywords-cpp-std-container-typedef-face)
    ;; container (and adaptor) members
    ("[.]\\_<\\(\
\\(?:r\\)?\\(begin\\|end\\)\\|\
\\(?:\\(?:push\\|pop\\)_\\)?\\(back\\|front\\)\\|\
insert\\|\\(?:max_\\|re\\)?size\\|empty\\|\
at\\|erase\\|clear\\|swap\\|assign\\|\
\\(c_\\|sub\\)str\\|append\\|replace\\|\
find\\(?:_\\(?:first\\|last\\)\\(?:_not\\)?_of\\)?\\|rfind\\|\
pop\\|top\\|push\\|\
count\\|\\(?:lower\\|upper\\)_bound\\|equal_range\\|\
compare\\|any\\|none\\|test\\|\\(?:re\\)?set\\|flip\\|to_ulong\
\\)\\_>"
     1 'dw-keywords-cpp-std-container-member-face prepend)
    ;; iterator
    ("::\\(\\(?:\\(?:const_\\)?\\(?:reverse_\\)?\\)?iterator\\)\\_>"
     1 'dw-keywords-cpp-std-iterator-face prepend)
    ;; genernal iterator
    ("\\(\\(?:front_\\|back_\\)?inserter\\|\\(?:[io]stream_\\)iterator\\)\\_>"
     1 'dw-keywords-cpp-std-iterator-face prepend)
    ;; standard library algorithm
    ("[^.]\\_<\\(\
find\\(?:_\\(?:if\\|first_of\\|end\\)\\)?\\|\
count\\(?:_if\\)?\\|\
adjacent_\\(?:find\\|difference\\)\\|\
search\\(?:_n\\)?\\|\
for_each\\|\
mismatch\\|\
equal\\(_range\\)?\\|\
\\(?:lower\\|upper\\)_bound\\|\
binary_search\\|\
fill\\(?:_n\\)?\\|\
generate\\(?:_n\\)?\\|\
copy\\(?:_backward\\)?\\|\
transform\\|\
replace\\(?:_copy\\)?\\(?:_if\\)?\\|\
merge\\|\
swap\\(?:_ranges\\)?\\|iter_swap\\|\
inplace_merge\\|\
stable_\\(?:partition\\|sort\\)\\|partition\\|sort\\|\
partial_sort\\(?:_copy\\)?\\|\
nth_element\\|\
remove\\(?:_if\\)?\\|\
unique\\(_copy\\)?\\|\
rotate\\(_copy\\)?\\|\
reverse\\(?:_copy\\)?\\|\
remove_copy\\(?:_if\\)?\\|\
random_shuffle\\|\
\\(?:next\\|prev\\)_permutation\\|\
includes\\|\
set_\\(?:union\\|intersection\\|\\(?:symmetric_\\)?difference\\)\\|\
min\\(_element\\)?\\|max\\(_element\\)?\\|\
lexicographical_compare\\|\
accumulate\\|\
inner_product\\|\
partial_sum\
\\)\\_>"
     1 'dw-keywords-cpp-std-algorithm-face prepend)
    ("\\_<\\(\
plus\\|minus\\|multiplies\\|divides\\|modulus\\|negate\\|\
equal_to\\|not_equal_to\\|\\(?:greater\\|less\\)\\(?:_equal\\)?\\|\
logical_\\(?:and\\|or\\|not\\)\
\\)\\_>"
     1 'dw-keywords-cpp-std-functor-face)
    ("\\_<\\(\
bind\\(?:1st\\|2nd\\)\\|not\\(?:1\\|2\\)\
\\)\\_>"
     1 'dw-keywords-cpp-std-functor-adaptor-face)
    ;; standard library io
    ;; io type
    ("\\_<\
\\(w?\\(?:[io]\\|io\\|[io]?\\(?:f\\|string\\)\\)stream\\)\
\\_>"
     1 'dw-keywords-cpp-std-io-face prepend)
    ;; io predefine
    ("\\_<\\(w?c\\(?:in\\|out\\|err\\|log\\)\\)\\_>"
     1 'dw-keywords-cpp-std-io-predef-face prepend)
    ;; io flag and manipulator
    ("\\_<\\(flush\\|end[ls]\\|\\(?:no\\)?unitbuf\\)\\_>"
     1 'dw-keywords-cpp-std-io-predef-face prepend)
    )

  "*A list of keywords in c++ standard library to highlight.")


(defcustom font-lock-keywords-cppprimer
  '()

  "*A list of keywords in C++ Primer to highlight.")


(defcustom font-lock-keywords-libboost
  '(
    ;; namespace
    ("\\_<\\(boost\\)\\_>"
     1 'dw-keywords-cpp-namespace-face prepend)
    )

  "*A list of keywords in c++ standard library to highlight.")


(defcustom font-lock-keywords-apue
  `(
    ;; functions
    ("\\_<\\(\
\\(?:err\\|log\\)_\\(?:dump\\|exit\\|msg\\|quit\\|ret\\|sys\\)\\|read\\|write\
\\)\\_>"
     1 'dw-keywords-apue-face)

    ;; ;; macros
    ;; ("\\(\\)"
    ;;  1 font-lock-preprocessor-face)

    ;; ;; types and structs
    ;; ("\\_<\\(\\)\\_>"
    ;;  1 font-lock-type-face)

    ;; ;; struct members names
    ;; ("\\_<\\(\\)\\_>"
    ;;  1 font-lock-variable-name-face)
    )

  "*A list of keywords of Advanced Programming in UNIX Environment
to highlight.")


(defcustom font-lock-keywords-unp
  '(
    ;; functions
    ("\\_<\\(\
hton[sl]\\|ntoh[sl]\\|b\\(?:zero\\|copy\\|cmp\\)\\|\
mem\\(?:set\\|cpy\\|cmp\\)\\|[iI]net_\\(?:[ap]ton\\|addr\\|nto[ap]\\)\\|\
[Ss]ock_\\(?:ntop\\(_host\\)?\\|bind_wild\\|cmp_\\(?:addr\\|port\\)\\|\
get_port\\|set_\\(?:addr\\|port\\|wild\\)\\)\\|[Rr]ead\\(?:n\\|line\\)?\\|\
[Ww]riten?\\|[Ff]\\(?:ge\\|pu\\)ts\\|\
[Ss]ocket\\|[Cc]onnect\\|[Bb]ind\\|[Ll]isten\\|[Aa]ccept\\|[Cc]lose\\|\
[Gg]et\\(?:sock\\|peer\\)name\\|[Ss]ignal\\|\
[Ss]elect\\|[Ss]hutdown\\|[Pp]select\\|[Pp]oll\\|\
[GgSs]etsockopt\\|[Ff]cntl\\|[Ii]octl\\|\
[Rr]ecvfrom\\|[Ss]endto\\|\
[Gg]ethostby\\(?:name\\|addr\\)\\(?:_r\\)\\|[Gg]etservby\\(?:name\\|port\\)\\|\
[Gg]etaddrinfo\\|[Ff]reeaddrinfo\\|[Gg]etnameinfo\\|\
[Hh]ost_serv\\|[Tt]cp_\\(?:connect\\|listen\\)\\|\
[Uu]dp_\\(?:client\\|connect\\|server\\)\\|\
[Gg]ethostbyname2\\|[Gg]etipnodebyname\\|[Ff]reehostent\\|\
syslog\\|openlog\\|closelog\\|daemon_init\\|daemon_inetd\
\\)\\_>"
     1 'dw-keywords-unp-face)

    ;; macros and variables
    ("\\_<\\(\
INET6?_ADDRSTRLEN\\|SIN6_LEN\\|AF_\\(?:INET6?\\|LOCAL\\|ROUTE\\|KEY\\)\\|\
SOCK_\\(?:STREAM\\|DGRAM\\|\\(?:SEQ\\)?PACKET\\|RAW\\)\\|\
IPPROTO_\\(?:TCP\\|UDP\\|SCTP\\)\\|INADDR_ANY\\|IN6ADDR_ANY_INIT\\|\
in6addr_any\\|\
FD_\\(?:ZERO\\|SET\\|CLR\\|ISSET\\|SETSIZE\\)\\|\
SHUT_\\(?:RD\\(?:WR\\)?\\|WR\\)\\|\
POLL\\(?:IN\\|\\(?:RD\\|WR\\)\\(?:NORM\\|BAND\\)\\|PRI\\|OUT\\|ERR\\|\
HUP\\|NVAL\\)\\|\
INFTIM\\|\
SOL_SOCKET\\|IPPROTO_\\(?:IP\\(?:V6\\)?\\|\\(?:ICMPV6\\)\\|TCP\\|SCTP\\)\\|\
SO_\\(BROADCAST\\|DEBUG\\|DONTROUTE\\|ERROR\\|KEEPALIVE\\|LINGER\\|\
OOBINLINE\\|\\(?:RCV\\|SND\\)\\(?:BUF\\|LOWAT\\|TIMEO\\)\\|\
REUSE\\(?:ADDR\\|PORT\\)\\|TYPE\\|USELOOPBACK\\)\\|\
IP_\\(?:HDRINCL\\|OPTIONS\\|RECVDSTADDR\\|RECVIF\\|TOS\\|TTL\\|\
MULTICAST_\\(?:IF\\|TTL\\|LOOP\\)\\|\
\\(?:ADD\\|DROP\\)\\(?:_SOURCE\\)?_MEMBERSHIP\\|\
\\(?:UN\\)?BLOCK_SOURCE\\)\\|\
ICMP6_FILTER\\|\
IPV6_\\(?:CHECKSUM\\|DONTFRAG\\|NEXTHOP\\|PATHMTU\\|\
RECV\\(?:DSTOPTS\\|HOP\\(?:LIMIT\\|OPTS\\)\\|PATHMTU\\|PKTINFO\\|RTHDR\\|\
TCLASS\\)\\|UNICAST_HOPS\\|USE_MIN_MTU\\|V6ONLY\\|\
MULTICAST_\\(?:IF\\|HOPS\\|LOOP\\)\\)\\|\\(?:JOIN\\|LEAVE\\)_GROUP\\|\
MCAST_\\(?:\\(?:JOIN\\|LEAVE\\)\\(?:_SOURCE\\)?_GROUP\\|\
\\(?:UN\\)?BLOCK_SOURCE\\)\\|\
TCP_\\(?:MAXSEG\\|NODELAY\\)\\|\
SCTP_\\(?:ADAPTION_LAYER\\|ASSOCINFO\\|AUTOCLOSE\\|DEFAULT_SEND_PARAM\\|\
DISABLE_FRAGMENTS\\|EVENTS\\|GET_PEER_ADDR_INFO\\|I_WAIT_MAPPED_V4_ADDR\\|\
INITMSG\\|MAX\\(?:BURST\\|SEG\\)\\|NODELAY\\|PEER_ADDR_PARAMS\\|\
\\(?:SET_PEER_\\)?PRIMARY_ADDR\\|RTOINFO\\|STATUS\\)\\|\
F_[GS]ET_\\(?:FL\\|OWN\\)\\|O_\\(NONBLOCK\\|ASYNC\\)\\|\
IN6_IS_ADDR_\\(?:UNSPECIFIED\\|LOOPBACK\\|MULTICAST\\|LINKLOCAL\\|SITELOCAL\\|\
V4\\(?:MAPPED\\|COMPAT\\)\\|MC_\\(?:\\(?:NODE\\|LINK\\|SITE\\|ORG\\)LOCAL\\|\
GLOBAL\\)\\)\\|\
LOG_\\(?:EMERG\\|ALERT\\|CRIT\\|ERR\\|WARNING\\|NOTICE\\|INFO\\|DEBUG\\)\\|\
LOG_\\(?:AUTH\\(?:PRIV\\)?\\|CRON\\|DAEMON\\|FTP\\|KERN\\|LOCAL[0-7]\\|LPR\\|\
MAIL\\|NEWS\\|SYSLOG\\|USER\\|UUCP\\)\\|\
LOG_\\(?:CONS\\|NDELAY\\|PERROR\\|PID\\)\
\\)\\_>"
     1 font-lock-preprocessor-face)

    ;; types and structs
    ("\\_<\\(\
sockaddr_in\\|socklen_t\\|sa_family_t\\|in_addr\\|\
sockaddr\\|\
socaddr_in6\\|in6_addr\\|\
sockaddr_storage\\|\
Sigfunc\\|\
fd_set\\|timeval\\|timespec\\|sigset_t\\|\
pollfd\\|\
linger\
\\)\\_>"
     1 font-lock-type-face)

    ;; struct members names
    ("\\_<\\(\
sin_len\\|sin_family\\|sin_port\\|sin_addr\\|sin_zero\\|s_addr\\|\
sa_len\\|sa_family\\|sa_data\\|\
sin6_len\\|sin6_family\\|sin6_port\\|sin6_flowinfo\\|sin6_addr\\|\
sin6_scope_id\\|s6_addr\\|\
ss_len\\|ss_family\\|\
tv_sec\\|tv_usec\\|\
tv_nsec\\|\
fd\\|events\\|revents\\|\
l_\\(?:onoff\\|linger\\)\
\\)\\_>"
     1 font-lock-variable-name-face)
    )

  "*A list of keywords of UNIX Network Programming to highlight.")


(defcustom font-lock-keywords-java
  `(;; TODO add keywords for java
    )

  "*A list of keywords to highlight in java mode.")


(defcustom font-lock-keywords-makefile
  '(
    ;; highlight simply expanded variable initialization, immediately evaluated.
    (":=" 0 'dw-assign-operator-face)
    ;; highlight recursively expanded variable initialization,
    ;; whose evaluation is deferred.
    ("[^:?+]\\(=\\)" 1 font-lock-string-face append)
    ;; highlight conditional variable assignment operator,
    ;; which evaluation is deferred.
    ("?=" 0 font-lock-string-face append)
    ;; highlight append variable assignment operator, whose evaluation is
    ;; either immediate or deferred (it depends on that
    ;; the variable is simple or recursive).
    ("\\(+\\)\\(=\\)"
     (1 'dw-assign-operator-face)
     (2 font-lock-string-face))

    ;; highlight parentheses in my favorite faces
    ("[](){}<>[]" . 'dw-pair-face)
    ;; highlight variables surrounding parentheses in pair
    ;; so the one not in pair will be highlighted differently as
    ;; `dw-pair-face', as configured previously.
    (dw-match-makefile-left-paren
     (0 'dw-makefile-paren-face prepend))
    (dw-match-makefile-right-paren
     (0 'dw-makefile-paren-face prepend))

    ;; override the makefile mode default setting for command line
    (makefile-match-action
     (1 font-lock-builtin-face prepend)
     (2 'makefile-shell prepend t)
     (3 'font-lock-string-face prepend t))
    ;; override the default setting for single character variables
    ("[^$]\\(\\$\\(?:[@%<?^+*_]\\|[a-zA-Z0-9]\\>\\)\\)"
     1 font-lock-builtin-face prepend)
    ;; add variants of built-in single character variable
    ("[^$]\\(\\$[({][@%<?^+*_][DF][)}]\\)" 1 font-lock-builtin-face prepend)
    ;; override the default setting for target related single character variables
    ;; it will override the previous line setting as well
    ("[^$]\\(\\$[@%*]\\)" 1 'makefile-targets prepend)
    ;; add variants of built-in target related single character variables
    ("[^$]\\(\\$[({][@%*][DF][)}]\\)" 1 'makefile-targets prepend)

    ;; add built-in function names
    (dw-match-makefile-built-in-function
     1 font-lock-builtin-face prepend t)
    )

  "*A list of keywords to highlight in makefile modes.")


(defcustom font-lock-keywords-script
  '(("[](){}<>[]" . 'dw-pair-face)
    )

  "*A list of keywords to highlight in shell/python modes.")


(defcustom font-lock-keywords-yasnippet
  '(;; change the default setting of variable
    ;; TODO need to comment the default settings code to enable this
    ;;      for UNKNOWN reasons. TO BE FIGURED OUT
    ("\\$\\([0-9]+\\)"
     (0 font-lock-function-name-face prepend)
     (1 font-lock-constant-face prepend t))
    ("\\${\\([0-9]+\\)\\(?::\\([^}]*\\)\\)?}"
     (0 font-lock-function-name-face prepend)
     (1 font-lock-constant-face prepend t)
     (2 font-lock-variable-name-face prepend t))
    ("\\$([)]*)"
     font-lock-preprocessor-face prepend)
    )

  "*A list of keywords to highlight in yas snippet editing mode.")


(defun font-lock-settings ()
  "Settings for `font-lock-mode'."

  ;; 打开font-lock-mode，用各种颜色高亮关键字
  (global-font-lock-mode t)

  ;; 让emacs最大限度"彩色化"文本
  (setq font-lock-maximum-decoration t)


  ;; 对不同的mode分别添加自定义的关键字高亮
  ;; all dev modes
  (dolist (mode dev-mode-list)
    (font-lock-add-keywords mode
                            font-lock-keywords-dev
                            'ADD-TO-END))

  ;; elisp/lisp/scheme modes in `wlc'


  ;; c/c++ modes
  (dolist (mode
           '(c-mode c++-mode))
    (dolist (keywords
             '(font-lock-keywords-cc
               font-lock-keywords-libc
               font-lock-keywords-apue
               font-lock-keywords-unp))
      (font-lock-add-keywords mode
                              (eval keywords)
                              'ADD-TO-END)))
  ;; particular to c++
  (dolist (keywords
           '(font-lock-keywords-libcpp
             font-lock-keywords-libboost))
    (font-lock-add-keywords 'c++-mode
                            (eval keywords)
                            'ADD-TO-END))


  ;; java mode
  (font-lock-add-keywords 'java-mode
                          font-lock-keywords-java
                          'ADD-TO-END)


  ;; makefile mode
  (dolist (mode
           '(makefile-mode
             makefile-gmake-mode))
    (font-lock-add-keywords mode
                            font-lock-keywords-makefile
                            'ADD-TO-END))


  ;; sh/python modes
  (dolist (mode
           '(sh-mode shell-mode python-mode))
    (font-lock-add-keywords mode
                            font-lock-keywords-script
                            'ADD-TO-END))


  ;; yas snippet mode
  (font-lock-add-keywords 'snippet-mode
                          font-lock-keywords-yasnippet
                          'ADD-TO-END)

  ;; 增加每次font-lock fontify函数更新的字符数
  ;;(setq jit-lock-chunk-size 1000)

  )


(provide 'font-lock-settings)
