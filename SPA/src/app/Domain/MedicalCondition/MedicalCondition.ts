export interface MedicalCondition {
  domainId?: string;
  code: string;
  designation: string;
  description: string;
  symptomsList?: string[];
}
