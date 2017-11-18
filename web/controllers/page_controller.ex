defmodule EeChat.PageController do
  use EeChat.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
