defmodule EeChat.PageControllerTest do
  use EeChat.ConnCase

  test "GET /", %{conn: conn} do
    conn = get conn, "/"
    assert html_response(conn, 200) =~ "Welcome to EEChat"
  end
end
