defmodule Test do
  defmacro run(a) do
    quote do
      def foo do
        <<unquote(a)>>
      end
    end
  end
end

defmodule A do
  require Test
  Test.run(<<1::size(8), <<69>>::binary>>)
end

IO.puts(A.foo())
