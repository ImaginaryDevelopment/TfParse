//cspell: ignore IYKYK, hostnames
module TfParse.Samples

open BReuse

type ValidatorType = PositionExpectation of (FParsec.Position -> bool)

type FullTestSample<'t> = {
    Input: string
    Expected: Result<'t option, unit>
    Validators: ValidatorType list
}

type TestFailure = {
    Message: string
    Detail: string
    Position: FParsec.Position
    State: string list
}

type TestResult = {
    Index: int
    Result: Result<string, TestFailure>
    Input: string
}

let ft items =
    items
    |> List.map (fun (i, exp, v) -> {
        Input = i
        Expected = exp
        Validators = v
    })

let expectPosition i =
    PositionExpectation(fun pos -> pos.Index = i)

let wsSamples: FullTestSample<unit> list =
    ft [
        "", Ok None, [ expectPosition 0 ]
        " ", Ok None, [ expectPosition 1 ]
        "\t", Ok None, [ expectPosition 1 ]
        "\r", Ok None, [ expectPosition 1 ]
        "\r\n", Ok None, [ expectPosition 2 ]
        "\n", Ok None, [ expectPosition 1 ]
    ]

let intSamples: FullTestSample<int> list =
    ft [
        "0", Ok(Some 0), [ expectPosition 1 ]
        "1,", Ok(Some 1), [ expectPosition 1 ]
        "1,2", Ok(Some 1), [ expectPosition 1 ]
        // this is a float, but the int parsers behavior doesn't mind
        "1.2", Ok(Some 1), [ expectPosition 1 ]
    ]

let idiSamples: FullTestSample<float> list =
    ft [
        "0.1", Ok(Some 0.1), List.empty
        "12.34", Ok(Some 12.34), List.empty
        "29.56", Ok(Some 29.56), List.empty
        "-3.0", Ok(Some -3.0), List.empty
    ]

let simpleVersion: FullTestSample<string> list =
    ft [
        "version = \"1.0.0\"", Ok(Some "1.0.0"), List.empty
        "version=\"1.0.0\"", Ok(Some "1.0.0"), List.empty
        "version= \"1.0.0\"", Ok(Some "1.0.0"), List.empty
        "version =\"1.0.0\"", Ok(Some "1.0.0"), List.empty
    ]

let simpleSettings = [ "version = \"1.0.0\""; "enabled = true" ]

let valuesSimple = [ "1"; "true"; "octopus"; "\"octopus\""; "0" ]
// let commaSepByExamples = [ "0,1", 2; "1,2", 2; "1,2,3", 3; "1, 2,3", 3; "1 ,2", 2 ]

let csvItems: FullTestSample<_> list =
    ft [
        let i', f' = Parsers.SettingValue.Int, Parsers.SettingValue.Float
        let isInt i = Ok(Some(i' i))
        "0", isInt 0, [ expectPosition 1 ]
        // trailing comma
        "1,", isInt 1, [ expectPosition 2 ]
        "2,", isInt 2, [ expectPosition 2 ]
        "3 , 2", isInt 3, [ expectPosition 4 ]

        "4 , 3,", isInt 4, [ expectPosition 4 ]

        "5 , 3,4 ,", isInt 5, [ expectPosition 4 ]

        trimEnd """6, "true" """, isInt 6, [ expectPosition 3 ]
        trimEnd """7,  "true" """, isInt 7, [ expectPosition 4 ]
        trimEnd "8, \t \"true\"", isInt 8, [ expectPosition 5 ]

        "\"test\",", Ok(Some(Parsers.SettingValue.Str "test")), List.empty
        // mixed list
        "10.23, -8.45, 8 ", Ok(Some(f' 10.23)), List.empty
    // multi-line mixed with trailing comma
    // empty list
    // "", 0
    ]

let csvTuple: FullTestSample<int * int> list =
    ft [
        "0,0", Ok(Some(0, 0)), [ expectPosition 3 ]
        "0,0, ", Ok(Some(0, 0)), [ expectPosition 5 ]
    ]

let csvExamples: FullTestSample<Parsers.SettingValue list> list =
    ft [
        let i', f' = Parsers.SettingValue.Int, Parsers.SettingValue.Float
        let isInt i = Ok(Some [ i' i ])
        let is' = List.map i' >> Some >> Ok
        "0 ", is' [ 0 ], []
        "1,2, ", is' [ 1; 2 ], []
        // trailing comma
        "2,", isInt 2, [ expectPosition 2 ]
        "3, 2", is' [ 3; 2 ], [ expectPosition 4 ]
        "4 , 2", is' [ 4; 2 ], [ expectPosition 5 ]

        "5 , 3,", is' [ 5; 3 ], [ expectPosition 6 ]

        "6 , 3,4 ,", is' [ 6; 3; 4 ], [ expectPosition 9 ]

        // 7
        """7,
          true """,
        Ok(
            Some[i' 7
                 Parsers.SettingValue.Bool true]
        ),
        List.empty

        // 8
        trim """ "test",""", Ok(Some[Parsers.SettingValue.Str "test"]), List.empty

        // 9, mixed list
        """9.23,
          -8.45,

          8
        """,
        Ok(Some [ f' 9.23; f' -8.45; i' 8 ]),
        List.empty

        // 10, multi-line mixed with trailing comma
        """10.23,
      -9.45,

      9,""",
        Ok(
            Some[f' 10.23
                 f' -9.45
                 i' 9]
        ),
        List.empty

    ]

let arrayExamples = [
    "[0,2]"
    "[ 1, 2 ]"
    "[ 2 , 2 ]"
    "[ 3, true]"
    "[ true, 4, ]"
    "[ true, 5,]"
]

let commentSamples = [ "# hi\n"; "// hi\r"; " # hello . _ world\r\n"; " # hello . _ world\n\r" ]

let lEqRSamples: FullTestSample<string * string> list =
    ft [
        "a=b", Ok(Some("a", "b")), [ expectPosition 3 ]
        "a = b", Ok(Some("a", "b")), [ expectPosition 5 ]
        "a =b", Ok(Some("a", "b")), [ expectPosition 4 ]
        "a= b", Ok(Some("a", "b")), [ expectPosition 4 ]
    ]

let breakOnTabLiteral text =
    match text |> String.indexOf "\\t" with
    | Some i -> failwithf "found tab literal at %i in '%s'" i text
    | None -> text

let simpleModule =
    """module "test" {
 source = "../"
 
 cidr_blocks = {
   eu-central-1a = "10.10.0.0/24"
 }
 
 environment = "test-${random_string.this.result}"
 name        = "test-${random_string.this.result}"
 vpc_id      = aws_vpc.test.id
}"""

let identifierSamples: FullTestSample<string> list =
    ft [
        "octopus", Ok None, [ expectPosition 7 ]
        "module.connector", Ok(Some "module.connector"), List.empty
        "id1", Ok None, [ expectPosition 3 ]
        "local.envs.dev", Ok(Some "local.envs.dev"), List.empty
    ]

let unquotedSettingSamples = [
    """octopus = octopus """
    """octopus = octopus"""
    """octopus =octopus"""
    """octopus=octopus"""
    "id1 = 2"
    "connector = module.connector"
    "   connector = module.connector"
    """octopus="octopus" """
    breakOnTabLiteral "octopus=\"octopus\""
    breakOnTabLiteral " octopus=\"octopus\"\t"
    breakOnTabLiteral "\toctopus=\"octopus\"\t"

    breakOnTabLiteral " \t octopus=\"octopus\" \t \r\n"
]

let tObjectBodySamples = [
    breakOnTabLiteral "\toctopus = octopus "
    """octopus = octopus"""
    """octopus=octopus"""
    " "

    """ octopus ="octopus" """

    """ octopus = octopus """

    """
      "Project" = jsonencode({
        SubSets = [
        ]
      })"""
]

let tObjectSamples = [
    """{
    }"""

    """{}"""

    """{
          octopus = octopus
          octopus=octopus
          "octopus" = "octopus"
      }  """

    """{
      "Project" = jsonencode({
        SubSets = [
        ]
      })
    }"""
]


let exampleDescription = [
    """
  description = <<- EOT
    ### Managed by Terraform Octopus

    <https://github.com/ABC-DEF/ADA.Octopus.tf-xyz-d>
  EOT"""
]

let exampleSettingBlocks = [
    """common_variables = {
  }
  """
    """common_variables = {
    "test" = "test2"
  }
  """
]

let quotedStringItems = [ "\"test\""; trimEnd """ "test1", "test2" """; """ "test3", """ ]

let quotedStringList = [

    """[
      "Test"
    ]"""

    """[

    ]"""

    """[
    ]"""

]

let jsonEncodeKvp = [

    "Test = 1"

    // 1
    """SubSets = [
    ]"""

    // 2
    """SubSets=[
    ]"""

    // 3
    """SubSets=[]"""


    // 4
    """SubSets=[1]"""

    // 5
    """SubSets=[{Port=200}] 4"""

    // 6
    """SubSets = [ { "Port" = 200 } ] 5"""

    // 7
    """SubSets = [
      {
        SubsetName = "ABC"
        Port = 200
      }
    ]"""

    // 8
    """SubSets = [
      {
        SubsetName = "ABC"
        Port = 200
      },
      {
        SubsetName = "DEF"
      }
    ]"""
]

let jsonObjects: FullTestSample<(string * Parsers.SettingValue) list> list =
    ft [
        """Test = 1""", Ok(Some [ "Test", Parsers.SettingValue.Int 1 ]), List.empty
        """SubSets = []""", Ok(Some [ "SubSets", Parsers.SettingValue.Array List.empty ]), [ expectPosition 12 ]
        """SubSets = [
        ]""",
        Ok(Some [ "SubSets", Parsers.SettingValue.Array List.empty ]),
        []
        """SubSets = [
          {
            SubsetName = "Hello"
            Port = 2000
          }
        ]""",
        Ok(
            Some [
                "SubSets",
                Parsers.SettingValue.Array [
                    Parsers.SettingValue.Other [
                        "SubsetName", Parsers.SettingValue.Str "Hello"
                        "Port", Parsers.SettingValue.Int 2000
                    ]
                ]
            ]
        ),
        []
    ]

let jsonEncodeExamples = [
    """jsonencode({
      SubSets= [
      ]
    })
    """

    """jsonencode({
      SubSets = [
      ]
    })
    """

    """jsonencode({
      SubSets = [
        1
      ]
    })
    """

    """jsonencode({
      SubSets= [
        {
          SubsetName = "Hello_ +-=/.,;:'Subset"
        }
      ]
    })
    """

    """jsonencode({
                Test = 1
              })
    """

]

let projectEnv = [
    let makeJsonEncodes i =
        jsonEncodeExamples
        |> List.mapi (fun i2 text ->
            $"""{{
          id = local.envs.dev
          vars = {{
            "id1" = {i + i2}
            "Project.Test" = "hello world"
            "Project.Tenant.Common.Subsets" = {text}
        }}""")

    """{
      id = local.envs.dev
      vars = {
      }
    }
    """

    """{
      id = local.envs.dev
      vars = {
        "id1" = 1
        "Project.Test" = "hello world"
      }}
    """

    """{
      id = local.envs.dev
      vars = {
        "id1" = 2
        "Project.Test" = "hello world"
      }}
    """

    """{
      id = local.envs.dev
      vars = {
        "id1" = 3
        "Project.Test" = "hello world"
        "Project.Tenant.Common.Subsets"=jsonencode({
          SubSets = "1"
        })
      }}
    """

    $"""{{
      id = local.envs.dev
      vars = {{
        "id1" = 4
        "Project.Test" = "hello world"
        "Project.Tenant.Common.Subsets" = {jsonEncodeExamples[0]}
    }}"""

    $"""{{
      id = local.envs.dev
      vars = {{
        "id1" = 5
        "Project.Test" = "hello world"
        "Project.Tenant.Common.Subsets" = {jsonEncodeExamples[0]}
    }}"""

    """{
      id = local.envs.dev
      vars = {
        "id1" = 6
        "Project.Test" = "hello world"
        "Project.Tenant.Common.Subsets" = jsonencode({
          SubSets = "1"
        })
      }}
    """

    """{
      id = local.envs.dev
      vars = {
        "id1" = 7
        "Project.Test" = "hello world"
        "Project.Tenant.Common.Subsets" = jsonencode({
          SubSets = 1
        })
      }}
    """

    """{
      id = local.envs.dev
      vars = {
        "id1" = 8
        "Project.Test" = "hello world"
        "Project.Tenant.Common.Subsets"   = jsonencode({
          SubSets = [
          ]
        })
      }}
    """

    yield! makeJsonEncodes 9
]

let exampleProj = [
    """{
    project = "Deploy My Components"
    envs = [
    ]
  }
  """

    """{
    project = "Test project"
    envs = [
      {
        id = local.envs.local
        vars = {
        }
      }
    ]
  }"""

    """{
    project = "Test project"
    envs = [
      {
        id = local.envs.local
        vars = {
          "var1" = "var1Value"
        }
      }
    ]
  }"""

]

let exampleProjectList = [
    """[
    {
      project = "hello pl 1"
      envs = [
      ]
    },
    {
      project = "hello pl2"
      envs = [
        {
          id = pl2.env1
          vars = {
          }
        }
      ]
    }
  ]"""

    """[
    {
      project = "hello pl 1"
      envs = [
      ]
    },
    {
      project = "hello pl2"
      envs = [
        {
          id = pl2.env1
          vars = {
            "pl2.env1.var1" = "v"
          }
        }
      ]
    }
  ]"""

    """[
  ]"""
]

let exampleAttachedProjectList = [
    """attached_projects = [
  ]"""

    """attached_projects = [
    {
      project = "Config - IYKYK"
      envs = [
      ]
    }
    ]"""

    """attached_projects = [
      {
        project = "Config - IYKYK"
        envs = [
        ]
      },
      {
        project = "apl 3"
        envs = [
          {
            id = local.apl3.env1
            vars = {
            }
          }
        ]
      }
    ]"""

    // 3
    """attached_projects = [
  ]"""

    // 4
    """attached_projects = [
      {
        project = "Test this mess"
        envs = [
        ]
      }
    ]"""

    // 5
    """attached_projects = [
      {
        project = "Test this mess"
        envs = [
          {
            id = main.proj1.env1
            vars = {
              "Project.ITest" = "https://abc_+-{}"
            }
          }
        ]
      }
    ]"""
    // 6
    """attached_projects = [
      {
        project = "Test this mess"
        envs = [
          {
            id = main.proj1.env1
            vars = {
              "Project.ITest" = "https://abc_+-{}"
              "Project.ITest.Second" = true
            }
          }
        ]
      }
    ]"""

    // 7
    """attached_projects = [
      {
        project = "Test this mess"
        envs = [
          {
            id = main.proj1.env1
            vars = {
              "Project.ITest" = "https://abc_+-{}"
              "Project.Tenant.Common.Subsets"  = jsonencode({
                Test = 1
              })
            }
          }
        ]
      }
    ]"""
    // 8
    """attached_projects = [
      {
        project = "Test this mess"
        envs = [
          {
            id = main.proj1.env1
            vars = {
              "Project.ITest" = "https://abc_+-{}"
              "Project.Tenant.Common.Subsets"  = jsonencode({
                Test = 1

              })
            }
          }
        ]
      }
    ]"""

    // 9
    """attached_projects = [
      {
        project = "Test this mess"
        envs = [
          {
            id = main.proj1.env1
            vars = {
              "Project.ITest" = "https://abc_+-{}"
              "Project.Tenant.Common.Subsets"  = jsonencode({
                "Test" = 1
              })
            }
          }
        ]
      }
    ]"""
]

let exampleMs = [
    """   # Optional: Add security group definitions"""
    """
    providers = {
    octopus = octopus
  }"""
    """octopus = octopus"""
    yield! exampleDescription
    yield! exampleAttachedProjectList
]

let exampleInput =
    """module "example" {
  source = "example-source"
  version = "1.0.0"
  providers = {
    octopus = octopus
  }

  description = <<- EOT
    ### Managed by Terraform Octopus

    <https://github.com/ABC-DEF/ADA.Octopus.tf-xyz-d>
  EOT

  connector = module.connector
  space_id = var.space_id
  display_name = "Mario Toasty"
  client_id = "123"

  # Optional: Additional routing
  enabled = true
  base_cidr_block = "10.0.0.0/8"
  servers = 3

  common_variables = {
    "Tenant.Common.Subset.Type" = "QA"
    "Tenant.Common"    = "D_#{Defaults.EnvName}"
  }

  tenant_tags = [
    "Tenant/Whatever"
  ]

  attached_projects = [
    {
      project = "Test this mess"
      envs = [
        {
          id = main.proj1.env1
          vars = {
            "Project.ITest" = "https://abc_+-{}"
            "Project.Tenant.Common.Subsets"  = jsonencode({
              Test = 1

            })
          }
        }
      ]
    }
  ]
}"""

let _examples = [
    """module "my_module" {
        source = "terraform-aws-modules/vpc/aws"
        version = "2.0.0"
    }"""

    """module "my_module" {
        source = "git::https://example.com/storage.git?ref=51d462976d84fdea54b47d80dcabbf680badcdb8"
        version = "2.0.0"
    }"""

    """module "vpc" {
  source = "terraform-aws-modules/vpc/aws"
  version = "3.0.0"
  name = "my-vpc"
  cidr = "10.0.0.0/16"
  enable_dns_support = true
  enable_dns_hostnames = true
  tags = {
    Environment = "production"
    Project     = "my-project"
  }
  # Optional: Specify custom VPC subnets
  enable_nat_gateway = true
  single_nat_gateway = true
  # Optional: Add security group definitions
  security_group_ids = [
    "sg-12345678",
    "sg-23456789"
  ]
  # Optional: Additional routing
  private_subnets = ["10.0.1.0/24", "10.0.2.0/24"]
  public_subnets  = ["10.0.3.0/24", "10.0.4.0/24"]
}
module "s3_bucket" {
  source = "terraform-aws-modules/s3-bucket/aws"
  version = "2.1.0"
  bucket = "my-unique-bucket-name"
  acl    = "private"
  tags = {
    Environment = "production"
    Project     = "my-project"
  }
}
module "iam_role" {
  source = "terraform-aws-modules/iam/aws//modules/iam-role"
  version = "2.0.0"
  role_name = "my-role"
  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Effect = "Allow"
        Principal = {
          Service = "ec2.amazonaws.com"
        }
        Action = "sts:AssumeRole"
      }
    ]
  })"""

]
