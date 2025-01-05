export interface DisplayMedicalConditionDTO {
  domainId?: string;
  code: string;
  designation: string;
  description: string;
  symptomsList?: string[];
}
