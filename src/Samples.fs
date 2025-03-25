//cspell: ignore IYKYK, hostnames
module TfParse.Samples

open BReuse


let wsSamples = [ ""; " "; "\t"; "\r"; "\r\n"; "\n" ]

let intSamples = [ "0"; "1,"; "1,2"; "1.2" ]

let simpleVersion = [
    "version = \"1.0.0\""
    "version=\"1.0.0\""
    "version= \"1.0.0\""
    "version =\"1.0.0\""
]

let simpleSettings = [ "version = \"1.0.0\""; "enabled = true" ]

let valuesSimple = [ "1"; "true"; "octopus"; "\"octopus\"" ]
let commaSepByExamples = [ "0,1", 2; "1,2", 2; "1,2,3", 3; "1, 2,3", 3; "1 ,2", 2 ]

let csvExamples = [
    "0", 1
    // trailing comma
    "1,", 1
    "2, 2 ", 2
    "3 , 2 ", 2
    "4 , 3, ", 2
    "5 , 3,4 , ", 3
    """ 6, "true" """, 2

    """ "test", """, 1
    // mixed list
    """ 8.23,
      -8.45,

      8
  """,
    3
    // multi-line mixed with trailing comma
    """ 9.23,
      -9.45,

      9,
  """,
    3
]

let arrayExamples = [ "[0,2]"; "[ 1, 2 ]"; "[ 2 , 2 ]"; "[ true, 4,]"; "[ true, 5, ]" ]

let commentSamples = [ "# hi\n"; "// hi\r"; " # hello . _ world\r\n"; " # hello . _ world\n\r" ]
let lEqRSamples = [ "a=b"; "a = b"; "a =b"; "a= b" ]

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

let identifierSamples = [ "octopus"; "module.connector" ]

let unquotedSettingSamples = [
    """octopus = octopus """
    """octopus = octopus"""
    """octopus =octopus"""
    """octopus=octopus"""
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

    // failing
    """ octopus ="octopus" """
    """ octopus = octopus """
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
]

let examples = [
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

let exampleQsItems = [ "test"; trimEnd """ "test1", "test2" """; """ "test3", """ ]

let exampleValueList = [
    """[
    "Test"
  ]"""

    """[

  ]"""

    """[
]"""

]

// let examplePEVars = [
//     """{
//   }"""

// ]

let examplePEnv = [
    """{
    id = local.envs.dev
    vars = {
    }}
  """

    """{
    id = local.envs.dev
    vars = {
      "Project.Test" = "hello world"
    }}
  """
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
            })
          }
        }
      ]
    }
  ]
}"""
