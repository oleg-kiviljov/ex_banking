defmodule ExBanking.User do
  use GenServer

  alias __MODULE__

  @enforce_keys [:name, :wallet]
  defstruct [:name, :wallet]

  def init(name) do
    {:ok, %User{name: name, wallet: MapSet.new()}}
  end

  def create(name) when is_binary(name) do
    GenServer.start_link(__MODULE__, name, [])
  end
end
