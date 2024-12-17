import {Mapper} from "../core/infra/Mapper";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import IMedicalConditionDTO from "../dto/IMedicalConditionDTO";
import {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IMedicalConditionPersistence} from "../dataschema/IMedicalConditionPersistence";

export class MedicalConditionMap extends Mapper<MedicalCondition> {

    public static toDTO(medicine: MedicalCondition): IMedicalConditionDTO {
        return {
            id: medicine.domainId.id.toValue(),
            condition: medicine.condition
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

    public static toPersistence (medicalCondition: MedicalCondition, id: number): IMedicalConditionPersistence {
        return {
            domainId: id,
            condition: medicalCondition.condition
        }
    }

}
