# add preview prints updated trustdown and does not write

    Code
      result <- withVisible(voucher:::add("bob"))
    Output
      # header
      alice
      bob

# add write updates file and emits cli success

    Code
      result <- withVisible(voucher:::add("carol", write = TRUE, default_platform = "github"))
    Message
      v Added (carol) to vouched contributors

# denounce preview trims reason and does not write

    Code
      result <- withVisible(voucher:::denounce("bob", reason = "  bad actor  "))
    Output
      # header
      alice
      -bob bad actor

# denounce write updates file and emits cli success

    Code
      result <- withVisible(voucher:::denounce("github:bob", write = TRUE))
    Message
      v Denounced (github:bob)

# check reports statuses via cli and returns invisibly

    Code
      vouched <- withVisible(voucher:::check("alice"))
    Message
      i alice is vouched
    Code
      denounced <- withVisible(voucher:::check("github:bob"))
    Message
      i github:bob is denounced
    Code
      unknown <- withVisible(voucher:::check("charlie"))
    Message
      i charlie is unknown

# check default_platform changes matching for unqualified handles

    Code
      github <- withVisible(voucher:::check("bob", default_platform = "github"))
    Message
      i bob is denounced
    Code
      gitlab <- withVisible(voucher:::check("bob", default_platform = "gitlab"))
    Message
      i bob is vouched
    Code
      none <- withVisible(voucher:::check("bob"))
    Message
      i bob is denounced

# check includes git blame author when blame is TRUE

    Code
      result <- withVisible(voucher:::check("alice", blame = TRUE))
    Message
      i alice is vouched (git blame: VoucherBlameTest)

# check errors when file is missing and handles empty file

    Code
      result <- withVisible(voucher:::check("nobody"))
    Message
      i nobody is unknown

