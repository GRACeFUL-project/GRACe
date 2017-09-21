FROM fpco/stack-build:lts-8.17

RUN curl -sSL https://github.com/MiniZinc/MiniZincIDE/releases/download/2.1.5/MiniZincIDE-2.1.5-bundle-linux-x86_64.tgz -o /MiniZinc.tar.gz\
   && tar -zxf MiniZinc.tar.gz \
   && rm -rf MiniZinc.tar.gz \
   && mv MiniZincIDE-2.1.5-bundle-linux-x86_64/ /MiniZinc \
   && cd /MiniZinc \
   && mv solns2out mzn2fzn fzn-gecode mzn-gecode minizinc /usr/bin/

ENV MZN_STDLIB_DIR /MiniZinc/share/minizinc
RUN mkdir /app
WORKDIR /app
ADD . /app
RUN stack install --system-ghc
ENTRYPOINT ["stack", "exec", "--allow-different-user", "--system-ghc", "--", "GRACeServer"]
