package bfcompiler.native

import java.util.StringJoiner

case class LdConfig(
    arch: String,
    additionalLibraries: Seq[String],
    additionalArguments: Seq[String] = Seq.empty
):
  def withLibrary(library: String): LdConfig =
    copy(additionalLibraries = additionalLibraries.appended(library))

  def withArgument(arg: String): LdConfig =
    copy(additionalArguments = additionalArguments.appended(arg))
  def toArgumentString: String =
    new StringJoiner(" ")
      .add(s"-arch $arch")
      .add(additionalLibraries.map(lib => s"-l$lib").mkString(" "))
      .add(additionalArguments.mkString(" "))
      .toString

object LdConfig:
  val arm64macos: LdConfig = LdConfig(
    arch = "arm64",
    additionalLibraries = Seq("System"),
    additionalArguments =
      Seq("-syslibroot", "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk")
  )
