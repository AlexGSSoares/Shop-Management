___________________________________________________

# STORE MANAGEMENT 
This programme uses JAVA (object-oriented programming paradigm) as the Frontend, OCAML (functional programming paradigm) and Prolog (logic programming paradigm) as the Backend.

The main class is "Main.java", where has all the main menu, and the sub-menus. Every step then calls the respective class, with the same name.
____________________________________
## PREREQUISITES 

Before you start, you'll need the following tools installed:
- Code editor: VS Code
- Java Development Kit (JDK)
- OCaml
- Dune (for building OCaml projects)
- Prolog
- JPL7 library (Swi-prolog integration)

>[!NOTE]
> Make sure that Java, OCaml and Prolog are properly configured and accessible from the terminal or command prompt.
____________________________________
## HOW TO BUILD 

#### OCAML
To build the OCaml file, run the following command in the terminal:

```
opam init \\to verify if Ocaml works perfectly
dune build \\to build the .exe 
```

>[!NOTE]
>the "dune" file in the ".../bin/dune" directory must have the following lines: 

```
(executable
 (public_name shop-management-)
  (name main) 
  (libraries shop-management- str))
 (env 
 (dev
   (flags (:standard -w -32 -w -33))))
```

#### JAVA
The file will appear on ".../bin" directory as "main.ml". 

To build the Java files, run the following command in the terminal: 

```
javac *.java
```

#### PROLOG


>[!NOTE]
>Set the environment variables with the comand: nano ~/.bashrc

```
export SWI_HOME_DIR=/usr/lib/swi-prolog/
export LD_LIBRARY_PATH=/usr/lib/swi-prolog/lib/x86_64-linux/:$LD_LIBRARY_PATH
export CLASSPATH=/usr/lib/swi-prolog/lib/jpl.jar:$CLASSPATH
export LD_PRELOAD=/usr/lib/swi-prolog/lib/x86_64-linux/libswipl.so
```

>[!NOTE]
> You need to add the referenced librarie: jpl.jar
___________________________________
## HOW TO RUN  

If you have done all until now, the programme is now ready to run, all you have to do is call the "Main.java" file in the terminal with the following command: 

``` 
java Main
```

Finally, the menu is intuitive and easy to use