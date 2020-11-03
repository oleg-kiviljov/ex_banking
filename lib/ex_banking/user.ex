defmodule ExBanking.User do
  use GenServer

  alias __MODULE__

  @enforce_keys [:username, :wallet]
  defstruct [:username, :wallet]

  def init(username) do
    {:ok, %User{username: username, wallet: MapSet.new()}}
  end

  def create(username) when is_binary(username) do
    GenServer.start_link(__MODULE__, username, name: via_tuple(username))
  end

  def create(_username) do
    {:error, :wrong_arguments}
  end

  def via_tuple(username), do: {:via, Registry, {Registry.User, username}}
end
