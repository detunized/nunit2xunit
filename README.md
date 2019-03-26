# NUnit to xUnit automatic conversion tool (PoC)

The code here a proof of concept, not a ready to use tool. But it works. It
converts a few types of NUnit `Assert` expressions to their equivalents in
xUnit. It also cleans up a couple of other small things, like `[Test]` to
`[Fact]` conversion or removal of `[TestFixture]`. 

I wrote about this project in detail [here][blog1], [here][blog2] and
[here][blog3].

## License

The library is released under [the MIT license][mit]. See [LICENSE][license]
for details.

[mit]: http://www.opensource.org/licenses/mit-license.php
[license]: LICENSE
[blog1]: https://detunized.net/posts/2019-03-12-nunit-to-xunit-automatic-test-conversion/
[blog2]: https://detunized.net/posts/2019-03-16-nunit-to-xunit-automatic-test-conversion-pattern-match/
[blog3]: https://detunized.net/posts/2019-03-26-nunit-to-xunit-automatic-test-conversion-code-rewrite/
