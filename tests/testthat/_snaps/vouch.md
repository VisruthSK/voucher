# use_vouch creates database and .Rbuildignore when missing

    Code
      expect_invisible(voucher:::use_vouch())
    Message
      i Wrote vouch database to '.github/VOUCHED.td'.

# use_vouch appends .github rule to existing .Rbuildignore

    Code
      expect_invisible(voucher:::use_vouch())
    Message
      i Wrote vouch database to '.github/VOUCHED.td'.

# use_vouch does not duplicate .github rule in .Rbuildignore

    Code
      expect_invisible(voucher:::use_vouch())
    Message
      i Wrote vouch database to '.github/VOUCHED.td'.

# use_vouch exits without changes when database already exists

    Code
      expect_invisible(voucher:::use_vouch())
    Message
      i Existing vouch database found at 'VOUCHED.td'. Exiting without any changes.

