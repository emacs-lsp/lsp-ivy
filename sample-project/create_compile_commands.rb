require 'json'
require 'shellwords'
db = %w[some_file.cpp some_other_file.cpp].map do |sourcefile|
  fullpath = File.join(Dir.pwd, sourcefile).shellescape
  { directory: Dir.pwd,
    file: fullpath,
    command: "clang++ #{fullpath}"
  }
end
File.open('./compile_commands.json', 'w') do |f|
  f.write JSON.pretty_generate(db)
end
