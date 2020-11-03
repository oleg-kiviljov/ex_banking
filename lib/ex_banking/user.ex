defmodule ExBanking.User do
  @enforce_keys [:name, :wallet]
  defstruct [:name, :wallet]
end
