"""
The module fetches ormolu binary to be used by bazel
"""

VERSION = "0.8.0.0"

WORKSPACE_WIDE_GHC_EXTENSIONS = [
    "ImportQualifiedPost",
]

URL = ("https://github.com/tweag/ormolu/releases/download/" +
       "{version}/ormolu-{arch}-{os}.zip")

SHA256 = {
    "x86_64-linux": "647a0c36d5675b9a7108f4bf4e6e703459c3ae33e18399f9ebce2ab4af295457",
}

ORMOLU_BUILD = """
exports_files(["ormolu"])
"""

def _ormolu_impl(repository_ctx):
    os_arch = repository_ctx.os.arch

    if os_arch == "x86_64" or os_arch == "amd64":
        pass
    else:
        fail("Unsupported architecture: " + os_arch)

    # For the purposes of ormolu binary, amd64 and x86_64 are the same.
    if os_arch == "amd64":
        os_arch = "x86_64"

    os_name = repository_ctx.os.name
    if os_name == "linux":
        platform = os_arch + "-linux"
    else:
        fail("Unsupported operating system: " + os_name)

    if platform not in SHA256:
        fail("Unsupported platform: " + platform)

    repository_ctx.report_progress("Fetching " + repository_ctx.name)
    repository_ctx.download_and_extract(
        url = URL.format(
            version = VERSION,
            arch = os_arch,
            os = os_name,
        ),
        sha256 = SHA256[platform],
    )
    repository_ctx.file("BUILD.bazel", ORMOLU_BUILD, executable = True)

_ormolu = repository_rule(
    implementation = _ormolu_impl,
    attrs = {},
)

def ormolu(name):
    _ormolu(name = name)

def ormolu_local_invocation(rule_def, name, mode, extra_args = {}):
    # Make sure that ormolu is invoked locally, as we expect it to be accessing all of the local
    # workspace content.
    if "tags" in extra_args:
        if "local" not in extra_args["tags"]:
            extra_args["tags"].append(["local"])
    else:
        extra_args["tags"] = ["local"]

    rule_def(
        name = name,
        srcs = ["ormolu.sh"],
        data = [
            "@ormolu",
        ],
        env = {
            "ormolu_mode": mode,
            "ormolu_path": "$(rootpath @ormolu//:ormolu)",
            "ormolu_extensions": ";".join(WORKSPACE_WIDE_GHC_EXTENSIONS),
        },
        **extra_args
    )
