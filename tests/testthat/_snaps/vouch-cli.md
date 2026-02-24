# add writes in-place when write is TRUE

    Code
      result <- withVisible(voucher:::add("github:carol", write = TRUE))
    Message
      v Added (github:carol) to vouched contributors

# check returns vouched denounced and unknown

    Code
      status_vouched <- voucher:::check("alice")
    Message
      i alice is vouched

---

    Code
      status_denounced <- voucher:::check("github:bob")
    Message
      i github:bob is denounced

---

    Code
      status_unknown <- voucher:::check("charlie")
    Message
      i charlie is unknown

# denounce previews and writes denounced entry

    Code
      result_write <- withVisible(voucher:::denounce("bob", reason = "bad actor",
        write = TRUE))
    Message
      v Denounced (bob)

# helpers cover vector and parsing edge branches

    Code
      status <- voucher:::check("alice")
    Message
      i alice is vouched

