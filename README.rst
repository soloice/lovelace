=============================
Lovelace Programming Language
=============================



Specification
=============


.. code::

    -- single line comment

    --[[
    multi-line
    comment
    ]]--

    use <<"stdlib.lol">>

    -- number
    1#char
    1#uchar
    1#uint16
    1#int32
    1#float
    1#double


    type day is
    enum
      sunday;
      monday;
      tuesday;
      wednesday;
      thursday;
      friday;
      saturday
    end

    -- XXX: type work_day is range monday ... friday of day

    type list(T) is
    enum
      nil;
      cons {T, list(T)}
    end

    List = cons#list(int32) {1, nil},

    case List of
      nil ->
        true#bool;
      cons {Head, Tail} ->
        false#bool
    end,

    List = #list(int32) [1],
    case List of
      [] -> true#bool;
      [Head|Tail] -> false#bool
    end,

    DoubleList = [ X * 2 || X <- List ],

    fun main(array(string)) -> void is
      (Args) ->
        print(<<"Hello, world!\n">>),
        void
    end

    -- better use proc for void

    proc main(array(string)) is
      (Args) ->
        print(<<"Hello, world!\n">>)
    end

    proc main(_) is
      (_) ->
        print(<<"Hello, world!\n">>)
    end




Roadmap
=======


* Hello, world!
* direct translation to C
* use LLVM as backend
* compiles to C after MLton-like optimization
