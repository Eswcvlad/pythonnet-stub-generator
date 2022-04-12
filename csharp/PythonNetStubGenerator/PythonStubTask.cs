﻿using System;
using System.IO;
using System.Linq;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;

namespace PythonNetStubGenerator
{
    public class PythonStubTask : Task
    {
        [Required]
        public string DestPath { get; set; }

        [Required]
        public string[] SourceDlls { get; set; }

        public string[] AdditionalSearchPaths { get; set; }

        public override bool Execute()
        {
            var destPath = new DirectoryInfo(DestPath);
            var sourceDlls = SourceDlls.Select(it => new FileInfo(it)).ToArray();
            var directoryPaths = AdditionalSearchPaths?.Select(it => new DirectoryInfo(it)).ToArray();

            try
            {
                StubBuilder.BuildAssemblyStubs(destPath, sourceDlls, directoryPaths);
            }
            catch (Exception e)
            {
                Log.LogError(e.Message);
                return false;
            }
            return true;
        }

    }
}