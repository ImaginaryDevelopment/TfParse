module TfParse.Samples

open BReuse

let simpleVersion = "version = \"1.0.0\""

let simpleSettings = [ "version = \"1.0.0\""; "enabled = true" ]

let valuesSimple = [ "1"; "true"; "octopus"; "\"octopus\"" ]

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

let exampleMs = [
    """   # Optional: Add security group definitions"""
    """
    providers = {
    octopus = octopus
  }"""
    """octopus = octopus"""
    yield! exampleDescription
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
}"""
