defmodule ExBanking do
  alias ExBanking.{User}

  def create_user(user) do
    User.create(user)
  end
end
