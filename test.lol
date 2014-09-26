fun sum(N, SUM) ->
   case N of
     0 ->
       SUM;
     _ ->
       sum(N-1, SUM+N)
   end
end

fun main() ->
    print(sum(5, 0))
end
