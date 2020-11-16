defmodule ExBankingTest do
  use ExUnit.Case
  doctest ExBanking

  @invalid_args [nil, 123, []]

  setup do
    {:ok, name: generate_name()}
  end

  describe "create_user/1 with valid args" do
    test "it creates user", %{name: name} do
      assert :ok == ExBanking.create_user(name)
    end
  end

  describe "create_user/1 with invalid args" do
    test "it returns :wrong_arguments error" do
      Enum.each(@invalid_args, fn arg ->
        assert {:error, :wrong_arguments} == ExBanking.create_user(arg)
      end)
    end
  end

  describe "create_user/1 with non-unique name" do
    test "it returns :user_already_exists error", %{name: name} do
      ExBanking.create_user(name)

      assert {:error, :user_already_exists} == ExBanking.create_user(name)
    end
  end

  describe "deposit/3 with valid args and new wallet" do
    test "it returns new balance of an user", %{name: name} do
      ExBanking.create_user(name)

      assert {:ok, 10} == ExBanking.deposit(name, 10, "EUR")
    end
  end

  describe "deposit/3 with valid args and existing wallet" do
    test "it returns new balance of an user", %{name: name} do
      ExBanking.create_user(name)

      assert {:ok, 10} == ExBanking.deposit(name, 10, "EUR")
      assert {:ok, 20} == ExBanking.deposit(name, 10, "EUR")
    end
  end

  describe "deposit/3 with invalid args" do
    test "it returns :wrong_arguments error", %{name: name} do
      Enum.each(@invalid_args, fn arg ->
        assert {:error, :wrong_arguments} == ExBanking.deposit(arg, 10, "EUR")
        assert {:error, :wrong_arguments} == ExBanking.deposit(name, 10, arg)
      end)

      assert {:error, :wrong_arguments} == ExBanking.deposit(name, -10, "EUR")
    end
  end

  describe "deposit/3 with non-existing user" do
    test "it returns :user_does_not_exist error" do
      assert {:error, :user_does_not_exist} == ExBanking.deposit("non_existing_user", 10, "EUR")
    end
  end

  describe "withdraw/3 with valid args and enough balance in wallet" do
    test "it returns new balance of an user", %{name: name} do
      ExBanking.create_user(name)
      ExBanking.deposit(name, 10, "EUR")

      assert {:ok, 0} == ExBanking.withdraw(name, 10, "EUR")
    end
  end

  describe "withdraw/3 with invalid args" do
    test "it returns :wrong_arguments error", %{name: name} do
      Enum.each(@invalid_args, fn arg ->
        assert {:error, :wrong_arguments} == ExBanking.withdraw(arg, 10, "EUR")
        assert {:error, :wrong_arguments} == ExBanking.withdraw(name, 10, arg)
      end)

      assert {:error, :wrong_arguments} == ExBanking.withdraw(name, -10, "EUR")
    end
  end

  describe "withdraw/3 with valid args and not enough balance in wallet" do
    test "it returns :not_enough_money error", %{name: name} do
      ExBanking.create_user(name)

      assert {:error, :not_enough_money} == ExBanking.withdraw(name, 10, "EUR")
    end
  end

  describe "withdraw/3 with non-existing user" do
    test "it returns :user_does_not_exist error" do
      assert {:error, :user_does_not_exist} == ExBanking.withdraw("non_existing_user", 10, "EUR")
    end
  end

  describe "get_balance/2 with valid args and new wallet" do
    test "it returns balance of an user", %{name: name} do
      ExBanking.create_user(name)

      assert {:ok, 0} == ExBanking.get_balance(name, "EUR")
    end
  end

  describe "get_balance/2 with valid args and existing wallet" do
    test "it returns balance of an user", %{name: name} do
      ExBanking.create_user(name)
      ExBanking.deposit(name, 10, "EUR")

      assert {:ok, 10} == ExBanking.get_balance(name, "EUR")
    end
  end

  describe "get_balance/2 with invalid args" do
    test "it returns :wrong_arguments error", %{name: name} do
      Enum.each(@invalid_args, fn arg ->
        assert {:error, :wrong_arguments} == ExBanking.get_balance(name, arg)
        assert {:error, :wrong_arguments} == ExBanking.get_balance(arg, "EUR")
      end)
    end
  end

  describe "get_balance/2 with non-existing user" do
    test "it returns :user_does_not_exist error" do
      assert {:error, :user_does_not_exist} == ExBanking.get_balance("non_existing_user", "EUR")
    end
  end

  describe "send/4 with valid args and non-existing receiver wallet" do
    test "it returns sender balance, receiver balance" do
      sender = generate_name()
      receiver = generate_name()

      ExBanking.create_user(sender)
      ExBanking.create_user(receiver)

      ExBanking.deposit(sender, 10, "EUR")

      assert {:ok, 0, 10} == ExBanking.send(sender, receiver, 10, "EUR")
    end
  end

  describe "send/4 with valid args and existing receiver wallet" do
    test "it returns sender balance, receiver balance" do
      sender = generate_name()
      receiver = generate_name()

      ExBanking.create_user(sender)
      ExBanking.create_user(receiver)

      ExBanking.deposit(sender, 10, "EUR")
      ExBanking.deposit(receiver, 10, "EUR")

      assert {:ok, 0, 20} == ExBanking.send(sender, receiver, 10, "EUR")
    end
  end

  describe "send/4 with valid args and not enough sender balance" do
    test "it returns :not_enough_money error" do
      sender = generate_name()
      receiver = generate_name()

      ExBanking.create_user(sender)
      ExBanking.create_user(receiver)

      ExBanking.deposit(sender, 5, "EUR")

      assert {:error, :not_enough_money} == ExBanking.send(sender, receiver, 10, "EUR")
    end
  end

  describe "send/4 with invalid args" do
    test "it returns :wrong_arguments error", %{name: name} do
      Enum.each(@invalid_args, fn arg ->
        assert {:error, :wrong_arguments} == ExBanking.send(arg, name, 10, "EUR")
        assert {:error, :wrong_arguments} == ExBanking.send(name, arg, 10, "EUR")
      end)

      sender = generate_name()
      receiver = generate_name()

      assert {:error, :wrong_arguments} == ExBanking.send(sender, receiver, -10, "EUR")
    end
  end

  describe "send/4 with non-existing sender" do
    test "it returns :sender_does_not_exist error", %{name: name} do
      ExBanking.create_user(name)

      assert {:error, :sender_does_not_exist} == ExBanking.send("non_existing_sender", name, 10, "EUR")
    end
  end

  describe "send/4 with non-existing receiver" do
    test "it returns :receiver_does_not_exist error", %{name: name} do
      ExBanking.create_user(name)

      assert {:error, :receiver_does_not_exist} == ExBanking.send(name, "non_existing_receiver", 10, "EUR")
    end
  end

  def generate_name do
    Faker.Person.first_name()
  end
end
