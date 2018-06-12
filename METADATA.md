This lists the changes in the Obi metadata, for when we want to parse old logs
in the future.

## 2

- Remove `Obi.Index.start_time` and `end_time` and replace with `duration`
  that represents the interval only.
- Add various `omit_nil` annotations to lists that can be empty in the sexp.

## 1

- Initial version of the metadata sexp file.
