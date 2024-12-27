export interface MedicalConditionDTO {
  domainId?: string;
  code: string;
  designation: string;
  description: string;
  symptomsList?: string[];
}
