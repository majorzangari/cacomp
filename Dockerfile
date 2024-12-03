# This will be an ubuntu environment
FROM ubuntu:20.04

RUN apt-get update && apt-get install -y \
    nasm \
    gcc-x86-64-linux-gnu

WORKDIR /app

COPY main.asm /app/

RUN nasm -f elf64 main.asm -o main.o

RUN x86_64-linux-gnu-ld -o out main.o

CMD ["./out"]