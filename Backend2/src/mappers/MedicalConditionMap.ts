import {Mapper} from "../core/infra/Mapper";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";

export class MedicalConditionMap extends Mapper<MedicalCondition> {
    
    public static toDTO(medicalCondition: MedicalCondition): IMedicalConditionDTO {
        return {
            domainId: medicalCondition.domainId.id.toValue(),
            code: medicalCondition.code.value,
            designation: medicalCondition.designation.value,
            description: medicalCondition.description.value,
            symptomsList: medicalCondition.symptomsList


        } as IMedicalConditionDTO;
    }

    public static toDomain (medicalCondition: any | Model<IMedicalConditionPersistence & Document> ): MedicalCondition {

        const medicalConditionOrError = MedicalCondition.create(
            medicalCondition,
            new UniqueEntityID(medicalCondition.domainId)
        );

        medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';

        return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;
    }

    public static toPersistence (medicalCondition: MedicalCondition, id: Number): any {
        
        if(id === undefined) {
            return {
                domainId: medicalCondition.domainId.id.toValue(),
                code: medicalCondition.code.value,
                designation: medicalCondition.designation.value,
                description: medicalCondition.description.value,
                symptomsList: medicalCondition.symptomsList
            }
        }
        
        return {
            domainId: id,
            code: medicalCondition.code.value,
            designation: medicalCondition.designation.value,
            description: medicalCondition.description.value,
            symptomsList: medicalCondition.symptomsList
        }
    }

}
