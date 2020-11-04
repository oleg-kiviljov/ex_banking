defmodule ExBanking.Wallet do
  use GenServer

  alias __MODULE__
  alias Decimal, as: D

  @enforce_keys [:currency, :balance]
  defstruct [:currency, :balance]

  def init(currency) do
    {:ok, %Wallet{currency: currency, balance: D.new(0)}}
  end
end
