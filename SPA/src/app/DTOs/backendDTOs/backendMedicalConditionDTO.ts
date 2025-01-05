export interface BackendMedicalConditionDTO {
  domainId?: string;
  code: string;
  designation: string;
  description: string;
  symptomsList?: string[];
}
