export interface CreateMedicalConditionDTO {
  code: string;
  designation: string;
  description: string;
  symptomsList?: string[];
}
