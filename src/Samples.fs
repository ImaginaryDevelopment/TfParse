let simpleVersion = "version = \"1.0.0\""

let simpleSettings = [ "version = \"1.0.0\""; "enabled = true" ]

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

let exampleInput =
    """module "example" {
  source = "example-source"
  version = "1.0.0"
  providers = {
    octopus = octopus
  }
  # Optional: Additional routing
  enabled = true
  base_cidr_block = "10.0.0.0/8"
  servers = 3
}"""
